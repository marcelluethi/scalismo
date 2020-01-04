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
import scalismo.common.interpolation.FieldInterpolator
import scalismo.geometry.{EuclideanVector, NDSpace, Point}

import scala.reflect.ClassTag

/**
 * Defines a discrete set of values, where each associated to a point of the domain.
 */

class DiscreteField[D, A](val domain: DiscreteDomain[D], val data: IndexedSeq[A]) extends PartialFunction[PointId, A] { self =>

  def values: Iterator[A] = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  def valuesWithIds = values zip domain.pointIds
  def pointsWithValues = domain.points zip values
  def pointsWithIds = domain.points.zipWithIndex

  def foreach(f: A => Unit): Unit = values.foreach(f)
  /**
   * Returns a continuous field, where the value at each point is that of the closest point in the discrete set
   *
   */
  @deprecated("please use the [[interpolate]] method with a [[NearestNeighborInterpolator]] instead", "0.16")
  def interpolateNearestNeighbor(): Field[D, A] = Field(RealSpace[D], (p: Point[D]) => apply(domain.findClosestPoint(p).id))

  /**
   * Interpolates the discrete field using the given interpolator.
   * @param interpolator Implements an interpolation scheme (e.g. Nearest Neighbor, B-Spline, ...)
   * @return A continuous field of the same type.
   */
  def interpolate(interpolator: FieldInterpolator[D, A]): Field[D, A] = {
    interpolator.interpolate(this)
  }

  override def equals(other: Any): Boolean =
    other match {

      case that: DiscreteField[D, A] =>
        (that canEqual this) &&
          domain == that.domain &&
          data == that.data

      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[DiscreteField[D, A]]

}

object DiscreteField {
  def apply[D, A](domain: DiscreteDomain[D], data: IndexedSeq[A]): DiscreteField[D, A] = new DiscreteField[D, A](domain, data)


  private[scalismo] def createFromDenseVector[D, A](domain: DiscreteDomain[D], d: DenseVector[Double])(implicit vectorizer: Vectorizer[A]) = {
    val dim = vectorizer.dim
    val data = d.toArray.grouped(dim).map(e => vectorizer.unvectorize(DenseVector(e))).toIndexedSeq
    new DiscreteField[D, A](domain, data)
  }

  private[scalismo] def vectorize[D, A](field: DiscreteField[D, A])(implicit vectorizer: Vectorizer[A]): DenseVector[Double] = {
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

  private[scalismo] def vectorize[D, A](values: IndexedSeq[A])(implicit vectorizer: Vectorizer[A]): DenseVector[Double] = {
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

class DiscreteScalarField[D: NDSpace, A: Scalar: ClassTag](domain: DiscreteDomain[D], data: ScalarArray[A]) extends DiscreteField[D, A](domain, data) {
  //class DiscreteScalarField[D: NDSpace, A: Scalar: ClassTag](val domain: DiscreteDomain[D], private[scalismo] val data: ScalarArray[A]) extends DiscreteField[D, A] {

  /** map the function f over the values, but ensures that the result is scalar valued as well */
  def map[B: Scalar: ClassTag](f: A => B): DiscreteScalarField[D, B] = {
    new DiscreteScalarField(domain, data.map(f))
  }

  override def values = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  override def equals(other: Any): Boolean =
    other match {

      case that: DiscreteScalarField[D, A] =>
        (that canEqual this) &&
          domain == that.domain &&
          data == that.data

      case _ => false
    }

  override def canEqual(other: Any): Boolean =
    other.isInstanceOf[DiscreteField[D, A]]

  @deprecated("please use the [interpolate] method with a [NearestNeighborInterpolator] instead", "0.16")
  override def interpolateNearestNeighbor(): ScalarField[D, A] = {
    ScalarField(RealSpace[D], (p: Point[D]) => apply(domain.findClosestPoint(p).id))
  }
  override lazy val hashCode: Int = data.hashCode() + domain.hashCode()

}

object DiscreteScalarField {
  def apply[D: NDSpace, DDomain <: DiscreteDomain[D], A: Scalar: ClassTag](domain: DDomain, data: ScalarArray[A]): DiscreteScalarField[D, A] = {
    new DiscreteScalarField[D, A](domain, data)
  }

  def apply[D: NDSpace, A: Scalar: ClassTag](domain: DiscreteDomain[D], data: Traversable[A]): DiscreteScalarField[D, A] = {
    new DiscreteScalarField[D, A](domain, ScalarArray(data.toArray))
  }
}

