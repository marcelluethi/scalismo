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
package scalismo.common

import breeze.linalg.DenseVector
import scalismo.common.DiscreteField.{ScalarMeshField, ScalarVolumeMeshField}
import scalismo.common.interpolation.FieldInterpolator
import scalismo.geometry.{_3D, EuclideanVector, NDSpace, Point}
import scalismo.mesh.{TetrahedralMesh, TriangleMesh}

import scala.reflect.ClassTag

/**
 * Defines a discrete set of values, where each associated to a point of the domain.
 */
class DiscreteField[D, +DDomain[D] <: DiscreteDomain[D], A](val domain: DDomain[D], val data: IndexedSeq[A])
    extends PartialFunction[PointId, A] { self =>

  private val pointSet = domain.pointSet

  def values: Iterator[A] = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  def valuesWithIds = values zip pointSet.pointIds
  def pointsWithValues = pointSet.points zip values
  def pointsWithIds = pointSet.points.zipWithIndex

  def foreach(f: A => Unit): Unit = values.foreach(f)

  /**
   * Returns a continuous field, where the value at each point is that of the closest point in the discrete set
   *
   */
  @deprecated("please use the [[interpolate]] method with a [[NearestNeighborInterpolator]] instead", "0.16")
  def interpolateNearestNeighbor(): Field[D, A] =
    Field(RealSpace[D], (p: Point[D]) => apply(pointSet.findClosestPoint(p).id))

  /**
   * Interpolates the discrete field using the given interpolator.
   * @param interpolator Implements an interpolation scheme (e.g. Nearest Neighbor, B-Spline, ...)
   * @return A continuous field of the same type.
   */
  def interpolate(interpolator: FieldInterpolator[D, DDomain, A]): Field[D, A] = {
    interpolator.interpolate(this)
  }

  override def equals(other: Any): Boolean =
    other match {

      case that: DiscreteField[D, DDomain, A] =>
        (that canEqual this) &&
          domain == that.domain &&
          data == that.data

      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[DiscreteField[D, DDomain, A]]

}

object DiscreteField {

  type ScalarVolumeMeshField[Value] = DiscreteField[_3D, TetrahedralMesh, Value]
  implicit class ScalarVolumeMeshFieldOps[Value](df: DiscreteField[_3D, TetrahedralMesh, Value]) {
    def mesh: TetrahedralMesh[_3D] = df.domain
  }

  type ScalarMeshField[Value] = DiscreteField[_3D, TriangleMesh, Value]
  implicit class ScalarMeshFieldOps[Value: Scalar: ClassTag](df: DiscreteField[_3D, TriangleMesh, Value]) {
    def mesh: TriangleMesh[_3D] = df.domain
  }

  def apply[D, DDomain[D] <: DiscreteDomain[D], A](domain: DDomain[D],
                                                   data: IndexedSeq[A]): DiscreteField[D, DDomain, A] =
    new DiscreteField[D, DDomain, A](domain, data)

  private[scalismo] def createFromDenseVector[D, DDomain[D] <: DiscreteDomain[D], A](
    domain: DDomain[D],
    d: DenseVector[Double]
  )(implicit vectorizer: Vectorizer[A]) = {
    val dim = vectorizer.dim
    val data = d.toArray.grouped(dim).map(e => vectorizer.unvectorize(DenseVector(e))).toIndexedSeq
    new DiscreteField(domain, data)
  }

  private[scalismo] def vectorize[D, DDomain[D] <: DiscreteDomain[D], A](
    field: DiscreteField[D, DDomain, A]
  )(implicit vectorizer: Vectorizer[A]): DenseVector[Double] = {
    val dim = vectorizer.dim
    val fullDim = field.valuesWithIds.length * dim
    val M = DenseVector.zeros[Double](fullDim)
    for (i <- field.valuesWithIds) {
      val m = vectorizer.vectorize(i._1)
      for (x <- 0 until dim) {
        M(i._2.id * dim + x) = m(x)
      }
    }
    M
  }

  private[scalismo] def vectorize[D, A](
    values: IndexedSeq[A]
  )(implicit vectorizer: Vectorizer[A]): DenseVector[Double] = {
    val dim = vectorizer.dim
    val valuesWithIndex = values.zipWithIndex
    val fullDim = valuesWithIndex.length * dim
    val M = DenseVector.zeros[Double](fullDim)
    for (i <- valuesWithIndex) {
      val m = vectorizer.vectorize(i._1)
      for (x <- 0 until dim) {
        M(i._2 * dim + x) = m(x)
      }
    }
    M
  }
}

object ScalarMeshField {
  def apply[S: Scalar: ClassTag](mesh: TriangleMesh[_3D], data: Traversable[S]): ScalarMeshField[S] = {
    DiscreteField(mesh, ScalarArray(data.toArray))
  }
}

/**
 *
 */
class DiscreteScalarField[D: NDSpace, +DDomain[D] <: DiscreteDomain[D], A: Scalar: ClassTag](domain: DDomain[D],
                                                                                             data: ScalarArray[A])
    extends DiscreteField[D, DDomain, A](domain, data) {
  //class DiscreteScalarField[D: NDSpace, A: Scalar: ClassTag](val domain: PointSet[D], private[scalismo] val data: ScalarArray[A]) extends DiscreteField[D, A] {

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map[B: Scalar: ClassTag](f: A => B): DiscreteScalarField[D, DDomain, B] = {
    new DiscreteScalarField(domain, data.map(f))
  }

  override def values = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  override def equals(other: Any): Boolean =
    other match {

      case that: DiscreteScalarField[D, DDomain, A] =>
        (that canEqual this) &&
          domain == that.domain &&
          data == that.data

      case _ => false
    }

  override def canEqual(other: Any): Boolean =
    other.isInstanceOf[DiscreteField[D, DDomain, A]]

  override lazy val hashCode: Int = data.hashCode() + domain.hashCode()

}

object DiscreteScalarField {
  def apply[D: NDSpace, DDomain[D] <: DiscreteDomain[D], A: Scalar: ClassTag](
    domain: DDomain[D],
    data: ScalarArray[A]
  ): DiscreteScalarField[D, DDomain, A] = {
    new DiscreteScalarField(domain, data)
  }

  def apply[D: NDSpace, DDomain[D] <: DiscreteDomain[D], A: Scalar: ClassTag](
    domain: DDomain[D],
    data: Traversable[A]
  ): DiscreteScalarField[D, DDomain, A] = {
    new DiscreteScalarField(domain, ScalarArray(data.toArray))
  }
}

@deprecated(
  "This will be removed in future versions. Please use DiscreteField class instead (e.g. DiscreteField[_3D,Vector[_3D]] instead of DiscreteVectorField[_3D,_3D])",
  "since 0.15"
)
class DiscreteVectorField[D: NDSpace, DDomain[D] <: DiscreteDomain[D], DO: NDSpace](
  domain: DDomain[D],
  data: IndexedSeq[EuclideanVector[DO]]
) extends DiscreteField[D, DDomain, EuclideanVector[DO]](domain, data) {

  val pointSet = domain.pointSet

  override def values = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  override def interpolateNearestNeighbor(): VectorField[D, DO] = {

    VectorField(RealSpace[D], (p: Point[D]) => apply(pointSet.findClosestPoint(p).id))
  }

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map(f: EuclideanVector[DO] => EuclideanVector[DO]): DiscreteVectorField[D, DDomain, DO] =
    new DiscreteVectorField(domain, data.map(f))

  def asBreezeVector: DenseVector[Double] = {
    val d = implicitly[NDSpace[DO]].dimensionality
    val v = DenseVector.zeros[Double](pointSet.numberOfPoints * d)
    for ((pt, i) <- pointSet.pointsWithId) {
      v(i.id * d until (i.id + 1) * d) := data(i.id).toBreezeVector
    }
    v
  }

}
