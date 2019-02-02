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

import scalismo.common.Field.{DifferentiableImage}
import scalismo.geometry._
import scalismo.registration.{CanDifferentiate, Transformation}


/**
 * An image is simply a function from points to values, together with a domain on which the
 * function is defined.
 */
class Field[D : NDSpace, A](val domain : Domain[D], val f : Point[D] => A) extends Function1[Point[D], A] {
  self =>


  /** True if the image is defined at the given point */
  def isDefinedAt(pt: Point[D]): Boolean = domain.isDefinedAt(pt)

  /**
    * The value of the image at a given point.
    * if an image is accessed outside of its definition, an exception is thrown
    */
  override def apply(x: Point[D]): A = {
    if (!isDefinedAt(x)) throw new IllegalArgumentException(s"Point $x is outside the domain")
    f(x)
  }

  /**
    * Lifts the definition of the value function such that it is defined everywhere,
    * but yields none if the value is outside of the domain
    */
  def liftValues: (Point[D] => Option[A]) = {
    val optionF = { (x: Point[D]) =>
      if (self.isDefinedAt(x)) Some(self.f(x))
      else None
    }

    new Field[D, Option[A]](domain, optionF) {
    }
    optionF
  }


  def sample[NewDomain <: DiscreteDomain[D]](domain: NewDomain, outsideValue: A): DiscreteField[D, NewDomain, A] = {

    // TODO this should be parallelized
    val values : Iterator[A] = domain.points.map(pt => {
      if (isDefinedAt(pt)) f(pt)
      else outsideValue
    })
    DiscreteField[D, NewDomain, A](domain, values.toIndexedSeq)
  }

  def +(that: Field[D, A])(implicit scalar: Scalar[A]): Field[D, A] = {
    def f(x: Point[D]): A = scalar.fromDouble(scalar.toDouble(this.f(x)) + scalar.toDouble(that.f(x)))

    new Field[D, A](Domain.intersection[D](domain, that.domain), f)
  }

  /** subtract two images. The domain of the new image is the intersection of the domains of the individual images */
  def -(that: Field[D, A])(implicit scalar: Scalar[A]): Field[D, A] = {
    def f(x: Point[D]): A = scalar.fromDouble(scalar.toDouble(this.f(x)) - scalar.toDouble(that.f(x)))

    val newDomain = Domain.intersection[D](domain, that.domain)
    new Field[D, A](newDomain, f)
  }

  /** scalar multiplication of a vector field */
  def *(s: Double)(implicit scalar: Scalar[A]): Field[D, A] = {

    def f(x: Point[D]): A = scalar.fromDouble(scalar.toDouble(this.f(x)) * s)

    new Field[D, A](domain, f)
  }

  def compose(t: Point[D] => Point[D]): Field[D, A] = {
    def f(x: Point[D]) = this.f(t(x))

    val newDomain = Domain.fromPredicate[D]((pt: Point[D]) => isDefinedAt(t(pt)))
    new Field(newDomain, f)
  }

    /** applies the given function to the image values */
  def andThen(g: A => A): Field[D, A] = {
      new Field(domain, f andThen g)
    }


}


/**
  * Utility functions to create and manipulate images
  */
object Field {


  def apply[D : NDSpace, A](dom: Domain[D], fun: Point[D] => A) = new Field[D, A](dom, fun) {}

  /**
    * Lifts a function between pixel values such that it acts on image intensities.
    * This is useful to write functions that manipulate the image intensities.
    */
  def lift[D : NDSpace, A](fl: A => A): Field[D, A] => Field[D, A] = {
    img: Field[D, A] => {
      val f = (x: Point[D]) => fl(img.apply(x))
      new Field(img.domain, f)
    }
  }

  type Image[D, A] = Field[D, A]
  type DifferentiableImage[D, A] = DifferentiableField[D, A]

}

object Field1D {
  def apply[A](domain : Domain[_1D], f : Point[_1D] => A) : Field[_1D, A] = {
    new Field(domain, f)
  }
}

object Field2D {
  def apply[A](domain : Domain[_2D], f : Point[_2D] => A) : Field[_2D, A] = {
    new Field(domain, f)
  }
}

object Field3D {
  def apply[A](domain : Domain[_3D], f : Point[_3D] => A) : Field[_3D, A] = {
    new Field(domain, f)
  }
}



class DifferentiableField[D : NDSpace, A : Scalar](domain : Domain[D],
                                                    f : Point[D] => A,
                                                    df : Point[D] => EuclideanVector[D]) extends Field[D, A](domain, f) {

  def differentiate: Field[D, EuclideanVector[D]] = Field(domain, df)


  def compose(t: Transformation[D] with CanDifferentiate[D]): DifferentiableField[D, A] = {
    def f(x: Point[D]) = this.f(t(x))
    def dfx(x : Point[D]) : EuclideanVector[D] =  t.takeDerivative(x) * df(t(x))
    val newDomain = Domain.fromPredicate[D]((pt: Point[D]) => isDefinedAt(t(pt)))
    new DifferentiableField(newDomain, f, dfx)
  }

}

object DifferentiableImage1D {
  def apply[A : Scalar](domain : Domain[_1D], f : Point[_1D] => A, df : Point[_1D] => EuclideanVector[_1D])
  : DifferentiableImage[_1D, A ] = {

    new DifferentiableImage(domain, f, df)
  }
}

object DifferentiableImage2D {
  def apply[A : Scalar](domain : Domain[_2D], f : Point[_2D] => A, df : Point[_2D] => EuclideanVector[_2D])
  : DifferentiableImage[_2D, A] = {

    new DifferentiableImage(domain, f, df)
  }
}

object DifferentiableImage3D {
  def apply[A : Scalar](domain : Domain[_3D], f : Point[_3D] => A, df : Point[_3D] => EuclideanVector[_3D])
  : DifferentiableImage[_3D, A] = {

    new DifferentiableImage(domain, f, df)
  }
}