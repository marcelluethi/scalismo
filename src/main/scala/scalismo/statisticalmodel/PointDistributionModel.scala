/*
 * Copyright 2015 University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalismo.statisticalmodel

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common._
import scalismo.common.interpolation.TriangleMeshInterpolator
import scalismo.geometry.EuclideanVector._
import scalismo.geometry._
import scalismo.mesh._
import scalismo.registration.{RigidTransformation, Transformation}
import scalismo.statisticalmodel.DiscreteLowRankGaussianProcess.Eigenpair
import scalismo.utils.Random

/**
 * A StatisticalMeshModel is isomorphic to a [[DiscreteLowRankGaussianProcess]]. The difference is that while the DiscreteLowRankGaussianProcess
 * models defomation fields, the StatisticalMeshModel applies the deformation fields to a mesh, and warps the mesh with the deformation fields to
 * produce a new mesh.
 *
 * @see [[DiscreteLowRankGaussianProcess]]
 */
case class PointDistributionModel[D: NDSpace, PointRepr <: DeformableDomain[D, PointRepr]] private (
  reference: PointRepr,
  gp: DiscreteLowRankGaussianProcess[D, PointRepr, EuclideanVector[D]]
)(implicit vectorizer: Vectorizer[EuclideanVector[D]]) {

  /** @see [[scalismo.statisticalmodel.DiscreteLowRankGaussianProcess.rank]] */
  val rank = gp.rank

  /**
   * The mean shape
   * @see [[DiscreteLowRankGaussianProcess.mean]]
   */
  lazy val mean: PointRepr = reference.warp(gp.mean)

  /**
   * The covariance between two points of the  mesh with given point id.
   * @see [[DiscreteLowRankGaussianProcess.cov]]
   */
  def cov(ptId1: PointId, ptId2: PointId) = gp.cov(ptId1, ptId2)

  /**
   * draws a random shape.
   * @see [[DiscreteLowRankGaussianProcess.sample]]
   */
  def sample()(implicit rand: Random): PointRepr = reference.warp(gp.sample())

  /**
   * returns the probability density for an instance of the model
   * @param instanceCoefficients coefficients of the instance in the model. For shapes in correspondence, these can be obtained using the coefficients method
   *
   */
  def pdf(instanceCoefficients: DenseVector[Double]): Double = {
    val disVecField = gp.instance(instanceCoefficients)
    gp.pdf(disVecField)
  }

  /**
   * returns a shape that corresponds to a linear combination of the basis functions with the given coefficients c.
   *  @see [[DiscreteLowRankGaussianProcess.instance]]
   */
  def instance(c: DenseVector[Double]): PointRepr = reference.warp(gp.instance(c))

  /**
   *  Returns a marginal StatisticalMeshModel, modelling deformations only on the chosen points of the reference
   *
   *  This method proceeds by clipping the reference mesh to keep only the indicated point identifiers, and then marginalizing the
   *  GP over those points. Notice that when clipping, not all indicated point ids will be part of the clipped mesh, as some points may not belong
   *  to any cells anymore. Therefore 2 behaviours are supported by this method :
   *
   *  1- in case some of the indicated pointIds remain after clipping and do form a mesh, a marginal model is returned only for those points
   *  2- in case none of the indicated points remain (they are not meshed), a reference mesh with all indicated point Ids and no cells is constructed and a marginal
   *  over this new reference is returned
   *
   * @see [[DiscreteLowRankGaussianProcess.marginal]]
   */
  def marginal(ptIds: IndexedSeq[PointId]): PointDistributionModel[D, PointRepr] = {
    ???
  }

  /**
   * Returns a reduced rank model, using only the leading basis functions.
   *
   * @param newRank: The rank of the new model.
   */
  def truncate(newRank: Int): PointDistributionModel[D, PointRepr] = {
    PointDistributionModel(reference, gp.truncate(newRank))
  }

  /**
   * @see [[DiscreteLowRankGaussianProcess.project]]
   */
  def project(pointData: PointRepr) = {
    val displacements =
      reference.pointSet.points
        .zip(pointData.pointSet.points)
        .map({ case (refPt, tgtPt) => tgtPt - refPt })
        .toIndexedSeq
    val dvf =
      DiscreteField[D, PointRepr, EuclideanVector[D]](reference, displacements)
    reference.warp(gp.project(dvf))
  }

  /**
   * @see [[DiscreteLowRankGaussianProcess.coefficients]]
   */
  def coefficients(pointData: PointRepr): DenseVector[Double] = {
    val displacements =
      reference.pointSet.points
        .zip(pointData.pointSet.points)
        .map({ case (refPt, tgtPt) => tgtPt - refPt })
        .toIndexedSeq
    val dvf =
      DiscreteField[D, PointRepr, EuclideanVector[D]](reference, displacements)
    gp.coefficients(dvf)
  }

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posterior(Int, Point[_3D])], sigma2: Double)]], but the training data is defined by specifying the target point instead of the displacement vector
   */
  def posterior(trainingData: IndexedSeq[(PointId, Point[D])], sigma2: Double): PointDistributionModel[D, PointRepr] = {
    val trainingDataWithDisplacements = trainingData.map {
      case (id, targetPoint) => (id, targetPoint - reference.pointSet.point(id))
    }
    val posteriorGp = gp.posterior(trainingDataWithDisplacements, sigma2)
    PointDistributionModel(reference, posteriorGp)
  }

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posterior(Int, Point[_3D], Double)]]], but the training data is defined by specifying the target point instead of the displacement vector
   */
  def posterior(
    trainingData: IndexedSeq[(PointId, Point[D], MultivariateNormalDistribution)]
  ): PointDistributionModel[D, PointRepr] = {
    val trainingDataWithDisplacements = trainingData.map {
      case (id, targetPoint, cov) => (id, targetPoint - reference.pointSet.point(id), cov)
    }
    val posteriorGp = gp.posterior(trainingDataWithDisplacements)
    new PointDistributionModel[D, PointRepr](reference, posteriorGp)
  }

  /**
   * transform the statistical mesh model using the given rigid transform.
   * The spanned shape space is not affected by this operations.
   */
  def transform(rigidTransform: RigidTransformation[D]): PointDistributionModel[D, PointRepr] = {
    val newRef = reference.transform(rigidTransform)

    val newMean: DenseVector[Double] = {
      val newMeanVecs = for ((pt, meanAtPoint) <- gp.mean.pointsWithValues) yield {
        rigidTransform(pt + meanAtPoint) - rigidTransform(pt)
      }
      val data = newMeanVecs.map(_.toArray).flatten.toArray
      DenseVector(data)
    }

    val newBasisMat = DenseMatrix.zeros[Double](gp.basisMatrix.rows, gp.basisMatrix.cols)

    for ((Eigenpair(_, ithKlBasis), i) <- gp.klBasis.zipWithIndex) {
      val newIthBasis = for ((pt, basisAtPoint) <- ithKlBasis.pointsWithValues) yield {
        rigidTransform(pt + basisAtPoint) - rigidTransform(pt)
      }
      val data = newIthBasis.map(_.toArray).flatten.toArray
      newBasisMat(::, i) := DenseVector(data)
    }
    val newGp = new DiscreteLowRankGaussianProcess[D, PointRepr, EuclideanVector[D]](
      gp.domain.transform(rigidTransform),
      newMean,
      gp.variance,
      newBasisMat
    )

    PointDistributionModel(newRef, newGp)

  }

  /**
   * Warps the reference mesh with the given transform. The space spanned by the model is not affected.
   */
  def changeReference(t: Point[D] => Point[D]): PointDistributionModel[D, PointRepr] = {

    val newRef = reference.transform(Transformation(t))
    val newMean = gp.mean.pointsWithValues.map { case (refPt, meanVec) => (refPt - t(refPt)) + meanVec }
    val newMeanVec = DenseVector(newMean.map(_.toArray).flatten.toArray)
    val newGp = new DiscreteLowRankGaussianProcess[D, PointRepr, EuclideanVector[D]](newRef,
                                                                                     newMeanVec,
                                                                                     gp.variance,
                                                                                     gp.basisMatrix)
    PointDistributionModel(newRef, newGp)
  }

  /**
   * Changes the number of vertices on which the model is defined
   * @param targetNumberOfVertices  The desired number of vertices
   * @return The new model
   */
  def decimate(targetNumberOfVertices: Int): PointDistributionModel[D, PointRepr] = {
    ???
  }

}

object PointDistributionModel {

  /**
   * creates a StatisticalMeshModel by discretizing the given Gaussian Process on the points of the reference mesh.
   */
  def apply[D: NDSpace, PointRepr <: DeformableDomain[D, PointRepr]](
    reference: PointRepr,
    gp: LowRankGaussianProcess[D, EuclideanVector[D]]
  )(implicit vectorizer: Vectorizer[EuclideanVector[D]]): PointDistributionModel[D, PointRepr] = {
    val discreteGp = DiscreteLowRankGaussianProcess(reference, gp)
    new PointDistributionModel(reference, discreteGp)
  }

  /**
   * creates a StatisticalMeshModel from vector/matrix representation of the mean, variance and basis matrix.
   *
   * @see [[DiscreteLowRankGaussianProcess.apply(FiniteDiscreteDomain, DenseVector[Double], DenseVector[Double], DenseMatrix[Double]]
   */
  private[scalismo] def apply[D: NDSpace, PointRepr <: DeformableDomain[D, PointRepr]](
    reference: PointRepr,
    meanVector: DenseVector[Double],
    variance: DenseVector[Double],
    basisMatrix: DenseMatrix[Double]
  )(implicit vectorizer: Vectorizer[EuclideanVector[D]]): PointDistributionModel[D, PointRepr] = {
    val gp =
      new DiscreteLowRankGaussianProcess[D, PointRepr, EuclideanVector[D]](reference, meanVector, variance, basisMatrix)
    new PointDistributionModel(reference, gp)
  }
}
