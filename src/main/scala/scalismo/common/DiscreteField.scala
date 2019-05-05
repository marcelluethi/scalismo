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
import scalismo.geometry._
import scalismo.image.DiscreteImageDomain

/**
 * Defines a discrete set of values, where each associated to a point of the domain.
 */

case class DiscreteField[D : NDSpace, +DDomain <: DiscreteDomain[D], A](domain: DDomain, data: IndexedSeq[A]) extends PartialFunction[PointId, A] { self =>

  def values: Iterator[A] = data.iterator
  override def apply(ptId: PointId) = data(ptId.id)
  override def isDefinedAt(ptId: PointId) = data.isDefinedAt(ptId.id)

  def valuesWithIds = values zip domain.pointIds
  def pointsWithValues = domain.points zip values
  def pointsWithIds = domain.points.zipWithIndex

  def dimensionality : Int = NDSpace[D].dimensionality

  def foreach(f: A => Unit): Unit = values.foreach(f)

  def map[B](f : A => B) : DiscreteField[D, DDomain, B] = {
    copy(data = data.map(f))
  }

  /**
   * Interpolates the discrete field using the given interpolator.
   * @param interpolator Implements an interpolation scheme (e.g. Nearest Neighbor, B-Spline, ...)
   * @return A continuous field of the same type.
   */
  def interpolate[B](interpolator: FieldInterpolator[D, DDomain, A]): Field[D, A] = {
    interpolator.interpolate(this)
  }

  def resample[NewDomain <: DiscreteDomain[D]](newDomain: NewDomain, interpolator: FieldInterpolator[D, DDomain, A], outsideValue: A): DiscreteField[D, NewDomain, A] = {

    val contImg = interpolate(interpolator)
    contImg.sample(newDomain, outsideValue)
  }

}

object DiscreteField {

  type DiscreteImage[D, A] = DiscreteField[D, DiscreteImageDomain[D], A]

  implicit class DiscreteImageOps[D : NDSpace, A](discreteField : DiscreteField[D, DiscreteImageDomain[D], A]) {

    def apply(idx: IntVector[D]): A = discreteField.apply(discreteField.domain.pointId(idx))

    def isDefinedAt(idx: IntVector[D]): Boolean = {
      (0 until NDSpace[D].dimensionality)
        .foldLeft(true)((res, d) => res && idx(d) >= 0 && idx(d) < discreteField.domain.size(d))
    }
  }

  def apply[D : NDSpace, DDomain <: DiscreteDomain[D], A](domain: DDomain, data: IndexedSeq[A]): DiscreteField[D, DDomain, A] = new DiscreteField[D, DDomain, A](domain, data)


  private[scalismo] def createFromDenseVector[D : NDSpace, DDomain <: DiscreteDomain[D], A](domain: DDomain, d: DenseVector[Double])(implicit vectorizer: Vectorizer[A]) = {
    val dim = vectorizer.dim
    val data = d.toArray.grouped(dim).map(e => vectorizer.unvectorize(DenseVector(e))).toIndexedSeq
    new DiscreteField[D, DDomain, A](domain, data)
  }

  private[scalismo] def vectorize[D, DDomain <: DiscreteDomain[D], A](field: DiscreteField[D, DDomain, A])(implicit vectorizer: Vectorizer[A]): DenseVector[Double] = {
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

object DiscreteField1D {
  def apply[DDomain <: DiscreteDomain[_1D], A](domain: DDomain, data: IndexedSeq[A]) : DiscreteField[_1D, DDomain, A] = {
    new DiscreteField(domain, data)
  }

  def apply[DDomain <: DiscreteDomain[_1D], A](domain: DDomain, f : Point[_1D] => A) : DiscreteField[_1D, DDomain, A] = {
    val data = domain.points.map(f).toIndexedSeq
    new DiscreteField(domain, data)
  }
}

object DiscreteField2D {
  def apply[DDomain <: DiscreteDomain[_2D], A](domain: DDomain, data: IndexedSeq[A]) : DiscreteField[_2D, DDomain, A] = {
    new DiscreteField[_2D, DDomain, A](domain, data)
  }

  def apply[DDomain <: DiscreteDomain[_2D], A](domain: DDomain, f : Point[_2D] => A) : DiscreteField[_2D, DDomain, A] = {
    val data = domain.points.map(f).toIndexedSeq
    new DiscreteField[_2D, DDomain, A](domain, data)
  }
}

object DiscreteField3D {
  def apply[DDomain <: DiscreteDomain[_3D], A](domain: DDomain, data: IndexedSeq[A]) : DiscreteField[_3D, DDomain, A] = {
    new DiscreteField[_3D, DDomain, A](domain, data)
  }

  def apply[DDomain <: DiscreteDomain[_3D], A](domain: DDomain, f : Point[_3D] => A) : DiscreteField[_3D, DDomain, A] = {
    val data = domain.points.map(f).toIndexedSeq
    new DiscreteField(domain, data)
  }
}