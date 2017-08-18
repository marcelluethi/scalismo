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
import scalismo.geometry.{ Dim, NDSpace, Point, Vector }
import scalismo.mesh.Interpolator$

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag

/**
 * Defines a discrete set of values, where each associated to a point of the domain.
 */
class DiscreteField[D <: Dim, +Dom <: DiscreteDomain[D], A](val domain: Dom, val data: IndexedSeq[A]) extends PartialFunction[PointId, A] { self =>

  def values: Iterator[A] = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  def valuesWithIds = values zip domain.pointIds
  def pointsWithValues = domain.points zip values
  def pointsWithIds = domain.points.zipWithIndex

  def foreach(f: A => Unit): Unit = values.foreach(f)

  /**
   * Returns a continuous field, where the value at each point is that of the closest point in the discrete set
   * *
   */
  def interpolateNearestNeighbor(): Field[D, A] = Field(RealSpace[D], (p: Point[D]) => apply(domain.findClosestPoint(p).id))
  // TODO conceptually, we should have a map here too, but it becomes tricky to
  // do since the overloaded functions will all require their own version of map
  // Maybe a trick with CanBuildFrom and Builder, similar to the scala collections would be required.

  //  def interpolateGeneric[B](interpolator : Interpolate[D, A, self.type]) : Field[D, Interpolate[D, A, self.type]#B] = {
  //    Field(RealSpace[D], interpolator.evaluate(this))
  //  }

  def interpolate(interpolator: FieldInterpolator[D, Dom, A]): Field[D, A] = {
    interpolator.interpolate(this)
  }

  override def equals(other: Any): Boolean =
    other match {

      case that: DiscreteField[D, Dom, A] =>
        (that canEqual this) &&
          domain == that.domain &&
          data == that.data

      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[DiscreteField[D, Dom, A]]

}

object DiscreteField {
  def apply[D <: Dim, Dom <: DiscreteDomain[D], A](domain: Dom, data: IndexedSeq[A]): DiscreteField[D, Dom, A] = new DiscreteField[D, Dom, A](domain, data)

  private[scalismo] def createFromDenseVector[D <: Dim, Dom <: DiscreteDomain[D], A](domain: Dom, d: DenseVector[Double])(implicit vectorizer: Vectorizer[A]) = {
    val dim = vectorizer.dim
    val nElem = d.length / dim
    val data = d.toArray.grouped(dim).map(e => vectorizer.unvectorize(DenseVector(e))).toIndexedSeq
    new DiscreteField[D, Dom, A](domain, data)
  }

  private[scalismo] def vectorize[D <: Dim, Dom <: DiscreteDomain[D], A](field: DiscreteField[D, Dom, A])(implicit vectorizer: Vectorizer[A]): DenseVector[Double] = {
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

  private[scalismo] def vectorize[D <: Dim, A](values: IndexedSeq[A])(implicit vectorizer: Vectorizer[A]): DenseVector[Double] = {
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

/**
 *
 */

class DiscreteScalarField[D <: Dim: NDSpace, Dom <: DiscreteDomain[D], A: Scalar: ClassTag](domain: Dom, data: ScalarArray[A]) extends DiscreteField[D, Dom, A](domain, data) {
  //class DiscreteScalarField[D <: Dim: NDSpace, A: Scalar: ClassTag](val domain: DiscreteDomain[D], private[scalismo] val data: ScalarArray[A]) extends DiscreteField[D, A] {

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map[B: Scalar: ClassTag](f: A => B): DiscreteScalarField[D, Dom, B] = {
    new DiscreteScalarField(domain, data.map(f))
  }

  override def values = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  override def equals(other: Any): Boolean =
    other match {

      case that: DiscreteScalarField[D, Dom, A] =>
        (that canEqual this) &&
          domain == that.domain &&
          data == that.data

      case _ => false
    }

  override def canEqual(other: Any): Boolean =
    other.isInstanceOf[DiscreteField[D, Dom, A]]

  override def interpolateNearestNeighbor(): ScalarField[D, A] = {
    ScalarField(RealSpace[D], (p: Point[D]) => apply(domain.findClosestPoint(p).id))
  }
  override lazy val hashCode: Int = data.hashCode() + domain.hashCode()

}

object DiscreteScalarField {
  def apply[D <: Dim: NDSpace, Dom <: DiscreteDomain[D], A: Scalar: ClassTag](domain: Dom, data: ScalarArray[A]): DiscreteScalarField[D, Dom, A] = {
    new DiscreteScalarField[D, Dom, A](domain, data)
  }

  def apply[D <: Dim: NDSpace, Dom <: DiscreteDomain[D], A: Scalar: ClassTag](domain: Dom, data: Traversable[A]): DiscreteScalarField[D, Dom, A] = {
    new DiscreteScalarField[D, Dom, A](domain, ScalarArray(data.toArray))
  }
}

@deprecated("This will be removed in future versions. Please use DiscreteField class instead (e.g. DiscreteField[_3D,Vector[_3D]] instead of DiscreteVectorField[_3D,_3D])", "since 0.15")
class DiscreteVectorField[D <: Dim: NDSpace, Dom <: DiscreteDomain[D], DO <: Dim: NDSpace](domain: Dom, data: IndexedSeq[Vector[DO]]) extends DiscreteField[D, Dom, Vector[DO]](domain, data) {

  override def values = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  override def interpolateNearestNeighbor(): VectorField[D, DO] = {
    VectorField(RealSpace[D], (p: Point[D]) => apply(domain.findClosestPoint(p).id))
  }

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map(f: Vector[DO] => Vector[DO]): DiscreteVectorField[D, Dom, DO] = new DiscreteVectorField(domain, data.map(f))

  def asBreezeVector: DenseVector[Double] = {
    val d = implicitly[NDSpace[DO]].dimensionality
    val v = DenseVector.zeros[Double](domain.numberOfPoints * d)
    for ((pt, i) <- domain.pointsWithId) {
      v(i.id * d until (i.id + 1) * d) := data(i.id).toBreezeVector
    }
    v
  }

}

@deprecated("This will be removed in future versions. Please use DiscreteField class instead (e.g. DiscreteField[_3D,Vector[_3D]] instead of DiscreteVectorField[_3D,_3D])", "since 0.15")
object DiscreteVectorField {
  def apply[D <: Dim: NDSpace, Dom <: DiscreteDomain[D], DO <: Dim: NDSpace](domain: Dom, data: IndexedSeq[Vector[DO]]): DiscreteVectorField[D, Dom, DO] = {
    new DiscreteVectorField(domain, data)
  }

  /**
   * Create a discreteVectorField for the given domain, where the data is represented as a dense vector.
   * If n is the number o fpoints in the domain and d the dimensionality (DO),
   * the vector is ordered as (v_11, v_12, ... v_1d, ...v_n1, v_n2, v_nd)
   */
  def fromDenseVector[D <: Dim: NDSpace, Dom <: DiscreteDomain[D], DO <: Dim: NDSpace](domain: Dom,
    vec: DenseVector[Double]): DiscreteVectorField[D, Dom, DO] = {
    val dim = implicitly[NDSpace[DO]].dimensionality
    val vectors =
      for (v <- vec.toArray.grouped(dim))
        yield Vector.apply[DO](v)

    DiscreteVectorField[D, Dom, DO](domain, vectors.toIndexedSeq)
  }

}