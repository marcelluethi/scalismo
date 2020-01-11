package scalismo.statisticalmodel

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.linalg.svd.SVD
import breeze.numerics.sqrt
import scalismo.common.{DiscreteField, Field, PointId, UnstructuredPoints}
import scalismo.geometry.{_3D, EuclideanVector, Point}
import scalismo.mesh.TriangleMesh
import scalismo.numerics.{FixedPointsUniformMeshSampler3D, PivotedCholesky}
import scalismo.registration.RigidTransformation
import scalismo.statisticalmodel.DiscreteLowRankGaussianProcess.Eigenpair

import scalismo.mesh.{TetrahedralCell, TetrahedralList, TetrahedralMesh, TetrahedralMesh3D}
import scalismo.utils.Random

import scala.util.{Failure, Success, Try}

// FIXME, will be rem
case class StatisticalVolumeMeshModel private (
  referenceVolumeMesh: TetrahedralMesh[_3D],
  gp: DiscreteLowRankGaussianProcess[_3D, TetrahedralMesh[_3D], EuclideanVector[_3D]]
) {

  /** @see [[scalismo.statisticalmodel.DiscreteLowRankGaussianProcess.rank]] */
  val rank = gp.rank

  /**
   * The mean shape
   * @see [[DiscreteLowRankGaussianProcess.mean]]
   */
  lazy val mean: TetrahedralMesh[_3D] = warpReference(gp.mean)

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
  def instance(c: DenseVector[Double]): TetrahedralMesh[_3D] = warpReference(gp.instance(c))

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
  def marginal(
    ptIds: IndexedSeq[PointId],
    referenceMeshVolume: TetrahedralMesh[_3D],
    gp: DiscreteLowRankGaussianProcess[_3D, TetrahedralMesh[_3D], EuclideanVector[_3D]]
  ): StatisticalVolumeMeshModel = {
    ???
  }

  /**
   * Returns a reduced rank model, using only the leading basis functions.
   *
   * @param newRank: The rank of the new model.
   */
  def truncate(newRank: Int): StatisticalVolumeMeshModel = {
    new StatisticalVolumeMeshModel(referenceVolumeMesh, gp.truncate(newRank))
  }

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posterior(Int, Point[_3D])], sigma2: Double)]], but the training data is defined by specifying the target point instead of the displacement vector
   */
  def posterior(trainingData: IndexedSeq[(PointId, Point[_3D])], sigma2: Double): StatisticalVolumeMeshModel = {
    val trainingDataWithDisplacements = trainingData.map {
      case (id, targetPoint) => (id, targetPoint - referenceVolumeMesh.pointSet.point(id))
    }
    val posteriorGp = gp.posterior(trainingDataWithDisplacements, sigma2)
    new StatisticalVolumeMeshModel(referenceVolumeMesh, posteriorGp)
  }

  /**
   * Similar to [[DiscreteLowRankGaussianProcess.posterior(Int, Point[_3D], Double)]]], but the training data is defined by specifying the target point instead of the displacement vector
   */
  def posterior(
    trainingData: IndexedSeq[(PointId, Point[_3D], MultivariateNormalDistribution)]
  ): StatisticalVolumeMeshModel = {
    val trainingDataWithDisplacements = trainingData.map {
      case (id, targetPoint, cov) => (id, targetPoint - referenceVolumeMesh.pointSet.point(id), cov)
    }
    val posteriorGp = gp.posterior(trainingDataWithDisplacements)
    new StatisticalVolumeMeshModel(referenceVolumeMesh, posteriorGp)
  }

  /**
   * transform the statistical mesh model using the given rigid transform.
   * The spanned shape space is not affected by this operations.
   */
  def transform(rigidTransform: RigidTransformation[_3D]): StatisticalVolumeMeshModel = {
    val newRef = referenceVolumeMesh.transform(rigidTransform)

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
    val newGp = new DiscreteLowRankGaussianProcess[_3D, TetrahedralMesh[_3D], EuclideanVector[_3D]](
      gp.domain.transform(rigidTransform),
      newMean,
      gp.variance,
      newBasisMat
    )

    new StatisticalVolumeMeshModel(newRef, newGp)

  }

  /**
   * Warps the reference mesh with the given transform. The space spanned by the model is not affected.
   */
  def changeReference(t: Point[_3D] => Point[_3D]): StatisticalVolumeMeshModel = {

    val newRef = referenceVolumeMesh.transform(t)
    val newMean = gp.mean.pointsWithValues.map { case (refPt, meanVec) => (refPt - t(refPt)) + meanVec }
    val newMeanVec = DenseVector(newMean.map(_.toArray).flatten.toArray)
    val newGp = new DiscreteLowRankGaussianProcess[_3D, TetrahedralMesh[_3D], EuclideanVector[_3D]](newRef,
                                                                                                    newMeanVec,
                                                                                                    gp.variance,
                                                                                                    gp.basisMatrix)
    new StatisticalVolumeMeshModel(newRef, newGp)
  }

  /**
   * @see [[DiscreteLowRankGaussianProcess.project]]
   */
  def project(mesh: TriangleMesh[_3D]) = {
    val displacements = referenceVolumeMesh.pointSet.points
      .zip(mesh.pointSet.points)
      .map({ case (refPt, tgtPt) => tgtPt - refPt })
      .toIndexedSeq
    val dvf = DiscreteField[_3D, TetrahedralMesh[_3D], EuclideanVector[_3D]](referenceVolumeMesh, displacements)
    warpReference(gp.project(dvf))
  }

  /**
   * @see [[DiscreteLowRankGaussianProcess.coefficients]]
   */
  def coefficients(mesh: TetrahedralMesh[_3D]): DenseVector[Double] = {
    val displacements = referenceVolumeMesh.pointSet.points
      .zip(mesh.pointSet.points)
      .map({ case (refPt, tgtPt) => tgtPt - refPt })
      .toIndexedSeq
    val dvf = DiscreteField[_3D, TetrahedralMesh[_3D], EuclideanVector[_3D]](referenceVolumeMesh, displacements)
    gp.coefficients(dvf)
  }

  private def warpReference(vectorPointData: DiscreteField[_3D, TetrahedralMesh[_3D], EuclideanVector[_3D]]) = {
    val newPoints = vectorPointData.pointsWithValues.map { case (pt, v) => pt + v }
    TetrahedralMesh3D(UnstructuredPoints(newPoints.toIndexedSeq), referenceVolumeMesh.tetrahedralization)
  }
}

object StatisticalVolumeMeshModel {

  /**
   * creates a StatisticalMeshModel by discretizing the given Gaussian Process on the points of the reference mesh.
   */
  def apply(referenceMesh: TetrahedralMesh[_3D],
            gp: LowRankGaussianProcess[_3D, EuclideanVector[_3D]]): StatisticalVolumeMeshModel = {
    val discreteGp = DiscreteLowRankGaussianProcess(referenceMesh, gp)
    new StatisticalVolumeMeshModel(referenceMesh, discreteGp)
  }

  /**
   * creates a StatisticalMeshModel from vector/matrix representation of the mean, variance and basis matrix.
   *
   * @see [[DiscreteLowRankGaussianProcess.apply(FiniteDiscreteDomain, DenseVector[Double], DenseVector[Double], DenseMatrix[Double]]
   */
  private[scalismo] def apply(referenceMesh: TetrahedralMesh[_3D],
                              meanVector: DenseVector[Double],
                              variance: DenseVector[Double],
                              basisMatrix: DenseMatrix[Double]) = {
    val gp = new DiscreteLowRankGaussianProcess[_3D, TetrahedralMesh[_3D], EuclideanVector[_3D]](referenceMesh,
                                                                                                 meanVector,
                                                                                                 variance,
                                                                                                 basisMatrix)
    new StatisticalVolumeMeshModel(referenceMesh, gp)
  }

}
