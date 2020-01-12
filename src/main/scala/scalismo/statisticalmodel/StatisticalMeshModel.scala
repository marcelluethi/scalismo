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

import breeze.linalg.svd.SVD
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics.sqrt
import scalismo.common._
import scalismo.common.interpolation.TriangleMeshInterpolator
import scalismo.geometry.EuclideanVector._
import scalismo.geometry._
import scalismo.mesh._
import scalismo.numerics.{FixedPointsUniformMeshSampler3D, PivotedCholesky}
import scalismo.registration.RigidTransformation
import scalismo.statisticalmodel.DiscreteLowRankGaussianProcess.Eigenpair
import scalismo.statisticalmodel.dataset.DataCollection
import scalismo.statisticalmodel.dataset.DataCollection.TriangleMeshDataCollection
import scalismo.utils.Random

import scala.util.{Failure, Success, Try}

/**
 * A StatisticalMeshModel is isomorphic to a [[DiscreteLowRankGaussianProcess]]. The difference is that while the DiscreteLowRankGaussianProcess
 * models defomation fields, the StatisticalMeshModel applies the deformation fields to a mesh, and warps the mesh with the deformation fields to
 * produce a new mesh.
 *
 * @see [[DiscreteLowRankGaussianProcess]]
 */
case class StatisticalMeshModel private (referenceMesh: TriangleMesh[_3D],
                                         gp: DiscreteLowRankGaussianProcess[_3D, TriangleMesh, EuclideanVector[_3D]]) {

  /** @see [[scalismo.statisticalmodel.DiscreteLowRankGaussianProcess.rank]] */
  val rank = gp.rank

  /**
   * The mean shape
   * @see [[DiscreteLowRankGaussianProcess.mean]]
   */
  lazy val mean: TriangleMesh[_3D] = warpReference(gp.mean)

  /**
   * The covariance between two points of the  mesh with given point id.
   * @see [[DiscreteLowRankGaussianProcess.cov]]
   */
  def cov(ptId1: PointId, ptId2: PointId) = gp.cov(ptId1, ptId2)

  /**
   * draws a random shape.
   * @see [[DiscreteLowRankGaussianProcess.sample]]
   */
  def sample()(implicit rand: Random) = warpReference(gp.sample())

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
  def instance(c: DenseVector[Double]): TriangleMesh[_3D] = warpReference(gp.instance(c))

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
  def marginal(ptIds: IndexedSeq[PointId]): StatisticalMeshModel = {
    ???
  }

  /**
   * Returns a reduced rank model, using only the leading basis functions.
   *
   * @param newRank: The rank of the new model.
   */
  def truncate(newRank: Int): StatisticalMeshModel = {
    new StatisticalMeshModel(referenceMesh, gp.truncate(newRank))
  }

  /**
   * @see [[DiscreteLowRankGaussianProcess.project]]
   */
  def project(mesh: TriangleMesh[_3D]) = {
    val displacements =
      referenceMesh.pointSet.points.zip(mesh.pointSet.points).map({ case (refPt, tgtPt) => tgtPt - refPt }).toIndexedSeq
    val dvf =
      DiscreteField[_3D, TriangleMesh, EuclideanVector[_3D]](referenceMesh, displacements)
    warpReference(gp.project(dvf))
  }

  /**
   * @see [[DiscreteLowRankGaussianProcess.coefficients]]
   */
  def coefficients(mesh: TriangleMesh[_3D]): DenseVector[Double] = {
    val displacements =
      referenceMesh.pointSet.points.zip(mesh.pointSet.points).map({ case (refPt, tgtPt) => tgtPt - refPt }).toIndexedSeq
    val dvf =
      DiscreteField[_3D, TriangleMesh, EuclideanVector[_3D]](referenceMesh, displacements)
    gp.coefficients(dvf)
  }

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posterior(Int, Point[_3D])], sigma2: Double)]], but the training data is defined by specifying the target point instead of the displacement vector
   */
  def posterior(trainingData: IndexedSeq[(PointId, Point[_3D])], sigma2: Double): StatisticalMeshModel = {
    val trainingDataWithDisplacements = trainingData.map {
      case (id, targetPoint) => (id, targetPoint - referenceMesh.pointSet.point(id))
    }
    val posteriorGp = gp.posterior(trainingDataWithDisplacements, sigma2)
    new StatisticalMeshModel(referenceMesh, posteriorGp)
  }

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posterior(Int, Point[_3D], Double)]]], but the training data is defined by specifying the target point instead of the displacement vector
   */
  def posterior(
    trainingData: IndexedSeq[(PointId, Point[_3D], MultivariateNormalDistribution)]
  ): StatisticalMeshModel = {
    val trainingDataWithDisplacements = trainingData.map {
      case (id, targetPoint, cov) => (id, targetPoint - referenceMesh.pointSet.point(id), cov)
    }
    val posteriorGp = gp.posterior(trainingDataWithDisplacements)
    new StatisticalMeshModel(referenceMesh, posteriorGp)
  }

  /**
   * transform the statistical mesh model using the given rigid transform.
   * The spanned shape space is not affected by this operations.
   */
  def transform(rigidTransform: RigidTransformation[_3D]): StatisticalMeshModel = {
    val newRef = referenceMesh.transform(rigidTransform)

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
    val newGp = new DiscreteLowRankGaussianProcess[_3D, TriangleMesh, EuclideanVector[_3D]](
      gp.domain.transform(rigidTransform),
      newMean,
      gp.variance,
      newBasisMat
    )

    new StatisticalMeshModel(newRef, newGp)

  }

  /**
   * Warps the reference mesh with the given transform. The space spanned by the model is not affected.
   */
  def changeReference(t: Point[_3D] => Point[_3D]): StatisticalMeshModel = {

    val newRef = referenceMesh.transform(t)
    val newMean = gp.mean.pointsWithValues.map { case (refPt, meanVec) => (refPt - t(refPt)) + meanVec }
    val newMeanVec = DenseVector(newMean.map(_.toArray).flatten.toArray)
    val newGp = new DiscreteLowRankGaussianProcess[_3D, TriangleMesh, EuclideanVector[_3D]](newRef,
                                                                                            newMeanVec,
                                                                                            gp.variance,
                                                                                            gp.basisMatrix)
    new StatisticalMeshModel(newRef, newGp)
  }

  /**
   * Changes the number of vertices on which the model is defined
   * @param targetNumberOfVertices  The desired number of vertices
   * @return The new model
   */
  def decimate(targetNumberOfVertices: Int): StatisticalMeshModel = {

    val newReference = referenceMesh.operations.decimate(targetNumberOfVertices)
    val interpolator = TriangleMeshInterpolator[EuclideanVector[_3D]]()
    val newGp = gp.interpolate(interpolator)

    StatisticalMeshModel(newReference, newGp)
  }

  private def warpReference(vectorPointData: DiscreteField[_3D, TriangleMesh, EuclideanVector[_3D]]) = {
    val newPoints = vectorPointData.pointsWithValues.map { case (pt, v) => pt + v }
    TriangleMesh3D(UnstructuredPoints(newPoints.toIndexedSeq), referenceMesh.triangulation)
  }

}

object StatisticalMeshModel {

  /**
   * creates a StatisticalMeshModel by discretizing the given Gaussian Process on the points of the reference mesh.
   */
  def apply(referenceMesh: TriangleMesh[_3D],
            gp: LowRankGaussianProcess[_3D, EuclideanVector[_3D]]): StatisticalMeshModel = {
    val discreteGp = DiscreteLowRankGaussianProcess(referenceMesh, gp)
    new StatisticalMeshModel(referenceMesh, discreteGp)
  }

  /**
   * creates a StatisticalMeshModel from vector/matrix representation of the mean, variance and basis matrix.
   *
   * @see [[DiscreteLowRankGaussianProcess.apply(FiniteDiscreteDomain, DenseVector[Double], DenseVector[Double], DenseMatrix[Double]]
   */
  private[scalismo] def apply(referenceMesh: TriangleMesh[_3D],
                              meanVector: DenseVector[Double],
                              variance: DenseVector[Double],
                              basisMatrix: DenseMatrix[Double]) = {
    val gp = new DiscreteLowRankGaussianProcess[_3D, TriangleMesh, EuclideanVector[_3D]](referenceMesh,
                                                                                         meanVector,
                                                                                         variance,
                                                                                         basisMatrix)
    new StatisticalMeshModel(referenceMesh, gp)
  }

  /**
   *  @deprecated
   *  Biasmodel can now be approximated before and the augment method is just an addition of two lowrank Gaussian processes.
   *  Please use approximate the biasModel before as a LowRankGaussianProcess and use the new method to create the bias model.
   */

  @deprecated("Please use the new method augmentModel(model,biasModel : LowRankGaussianProcess)", "20-04-2016")
  def augmentModel(model: StatisticalMeshModel,
                   biasModel: GaussianProcess[_3D, EuclideanVector[_3D]],
                   numBasisFunctions: Int)(implicit rand: Random): StatisticalMeshModel = {

    val modelGP = model.gp.interpolateNearestNeighbor
    // TODO: check if there is a better alternative (move method to Field?)
    val newMean =
      Field[_3D, EuclideanVector[_3D]](modelGP.domain, (p: Point[_3D]) => modelGP.mean(p) + biasModel.mean(p))
    val newCov = modelGP.cov + biasModel.cov
    val newGP = GaussianProcess(newMean, newCov)
    val sampler = FixedPointsUniformMeshSampler3D(model.referenceMesh, 2 * numBasisFunctions)
    val newLowRankGP = LowRankGaussianProcess.approximateGPNystrom(newGP, sampler, numBasisFunctions)
    StatisticalMeshModel(model.referenceMesh, newLowRankGP)
  }

  /**
   *  Adds a bias model to the given statistical shape model
   */
  def augmentModel(model: StatisticalMeshModel, biasModel: LowRankGaussianProcess[_3D, EuclideanVector[_3D]]) = {

    val discretizedBiasModel = biasModel.discretize(model.referenceMesh)
    val eigenvalues = DenseVector.vertcat(model.gp.variance, discretizedBiasModel.variance).map(sqrt(_))
    val eigenvectors = DenseMatrix.horzcat(model.gp.basisMatrix, discretizedBiasModel.basisMatrix)

    for (i <- 0 until eigenvalues.length) {
      eigenvectors(::, i) :*= eigenvalues(i)
    }

    val l: DenseMatrix[Double] = eigenvectors.t * eigenvectors
    val SVD(v, _, _) = breeze.linalg.svd(l)
    val U: DenseMatrix[Double] = eigenvectors * v
    val d: DenseVector[Double] = DenseVector.zeros(U.cols)
    for (i <- 0 until U.cols) {
      d(i) = breeze.linalg.norm(U(::, i))
      U(::, i) := U(::, i) * (1.0 / d(i))
    }

    val r = model.gp.copy[_3D, TriangleMesh, EuclideanVector[_3D]](
      meanVector = model.gp.meanVector + discretizedBiasModel.meanVector,
      variance = breeze.numerics.pow(d, 2),
      basisMatrix = U
    )
    StatisticalMeshModel(model.referenceMesh, r)
  }

  /**
   * Returns a PCA model with given reference mesh and a set of items in correspondence.
   * All points of the reference mesh are considered for computing the PCA
   *
   * Per default, the resulting mesh model will have rank (i.e. number of principal components) corresponding to
   * the number of linearly independent fields. By providing an explicit stopping criterion, one can, however,
   * compute only the leading principal components. See PivotedCholesky.StoppingCriterion for more details.
   */
  def createUsingPCA(
    dc: TriangleMeshDataCollection,
    stoppingCriterion: PivotedCholesky.StoppingCriterion = PivotedCholesky.RelativeTolerance(0)
  ): Try[StatisticalMeshModel] = {
    Try {
      val pdm = PointDistributionModel.createUsingPCA(dc, stoppingCriterion)
      new StatisticalMeshModel(pdm.reference, pdm.gp)
    }
  }

  /**
   * Creates a new Statistical mesh model, with its mean and covariance matrix estimated from the given fields.
   *
   * Per default, the resulting mesh model will have rank (i.e. number of principal components) corresponding to
   * the number of linearly independent fields. By providing an explicit stopping criterion, one can, however,
   * compute only the leading principal components. See PivotedCholesky.StoppingCriterion for more details.
   *
   */
  def createUsingPCA(referenceMesh: TriangleMesh[_3D],
                     fields: Seq[Field[_3D, EuclideanVector[_3D]]],
                     stoppingCriterion: PivotedCholesky.StoppingCriterion): StatisticalMeshModel = {
    val pdm = PointDistributionModel.createUsingPCA(referenceMesh, fields, stoppingCriterion)
    new StatisticalMeshModel(pdm.reference, pdm.gp)
  }

}

object PointD
