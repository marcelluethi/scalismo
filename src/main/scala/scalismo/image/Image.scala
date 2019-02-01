///*
// * Copyright 2015 University of Basel, Graphics and Vision Research Group
// *
// * Licensed under the Apache License, Version 2.0 (the "License");
// * you may not use this file except in compliance with the License.
// * You may obtain a copy of the License at
// *
// *     http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//package scalismo.image
//
//import scalismo.common.DiscreteField.DiscreteImage
//import scalismo.common._
//import scalismo.geometry._
//import scalismo.image.filter.Filter
//import scalismo.numerics.{GridSampler, Integrator}
//import scalismo.registration.{CanDifferentiate, Transformation}
//
//import scala.reflect.ClassTag
//
///**
// * An image whose values are scalar.
// */
//class ScalarImage[D: NDSpace] protected (override val domain: Domain[D], override val f: Point[D] => Float) extends Field[D, Float](domain, f) {
//
//  /** adds two images. The domain of the new image is the intersection of both */
//  def +(that: ScalarImage[D]): ScalarImage[D] = {
//    def f(x: Point[D]): Float = this.f(x) + that.f(x)
//    new ScalarImage(Domain.intersection[D](domain, that.domain), f)
//  }
//
//  /** subtract two images. The domain of the new image is the intersection of the domains of the individual images*/
//  def -(that: ScalarImage[D]): ScalarImage[D] = {
//    def f(x: Point[D]): Float = this.f(x) - that.f(x)
//    val newDomain = Domain.intersection[D](domain, that.domain)
//    new ScalarImage(newDomain, f)
//  }
//
//  /** element wise multiplication. The domain of the new image is the intersection of the domains of the individual images*/
//  def :*(that: ScalarImage[D]): ScalarImage[D] = {
//    def f(x: Point[D]): Float = this.f(x) * that.f(x)
//    val newDomain = Domain.intersection[D](domain, that.domain)
//    new ScalarImage(newDomain, f)
//  }
//
//  /** scalar multiplication of an image */
//  override def *(s: Double): ScalarImage[D] = {
//    def f(x: Point[D]): Float = this.f(x) * s.toFloat
//    val newDomain = domain
//    new ScalarImage(newDomain, f)
//  }
//
//  /** composes (i.e. warp) an image with a transformation. */
//  def compose(t: Point[D] => Point[D]): ScalarImage[D] = {
//    def f(x: Point[D]) = this.f(t(x))
//
//    val newDomain = Domain.fromPredicate[D]((pt: Point[D]) => isDefinedAt(t(pt)))
//    new ScalarImage(newDomain, f)
//  }
//
//  /** applies the given function to the image values */
//  def andThen(g: Float => Float): ScalarImage[D] = {
//    new ScalarImage(domain, f andThen g)
//  }
//
//  /**
//   * Convolution of an image with a given filter. The convolution is carried out by
//   * numerical integration, using the given number of points elevated to the power of dimensionality as an approximation.
//   *
//   * @param filter Filter to be used in the convolution.
//   * @param  numberOfPointsPerDim Number of points to be used to approximate the filter. Depending on the
//   * support size of the filter and the Frequency of the image, increasing this value can help avoid artifacts (at the cost of heavier computation)
//   */
//  def convolve(filter: Filter[D], numberOfPointsPerDim: Int)(implicit c: CreateDiscreteImageDomain[D]): ScalarImage[D] = {
//
//    val dim = implicitly[NDSpace[D]].dimensionality
//    val supportSpacing = filter.support.extent * (1f / numberOfPointsPerDim.toFloat)
//    val supportSize = IntVector[D]((0 until dim).map(_ => numberOfPointsPerDim).toArray)
//    val origin = (supportSpacing * ((numberOfPointsPerDim - 1) * -0.5f)).toPoint
//
//    val support = DiscreteImageDomain[D](origin, supportSpacing, supportSize)
//    val lifted = liftValues
//
//    val integrator = Integrator[D](GridSampler(support))
//
//    def intermediateF(imageX: Point[D])(t: Point[D]): Option[Float] = {
//
//      val p = (imageX - t).toPoint
//      lifted(p).map(_ * filter(t))
//    }
//
//    def f(imageX: Point[D]) = {
//      integrator.integrateScalar(intermediateF(imageX) _)
//    }
//
//    ScalarImage(domain, f)
//  }
//
//  /**
//   * Returns a discrete scalar image with the given domain, whose values are obtained by sampling the scalarImage at the domain points.
//   * If the image is not defined at a domain point, the outside value is used.
//   */
//  def sample[Pixel: Scalar: ClassTag](domain: DiscreteImageDomain[D], outsideValue: Float): DiscreteImage[D, Pixel] = {
//    val numeric = implicitly[Scalar[Pixel]]
//    val convertedOutsideValue = numeric.fromFloat(outsideValue)
//
//    val nbChunks = Runtime.getRuntime().availableProcessors() * 2
//    val parallelArrays = domain.pointsInChunks(nbChunks).par.map { chunkIterator =>
//      chunkIterator.map(pt => {
//        if (isDefinedAt(pt)) numeric.fromFloat(f(pt))
//        else convertedOutsideValue
//      }).toArray
//    }
//
//    DiscreteField[D, DiscreteImageDomain[D], Pixel](domain, ScalarArray(parallelArrays.reduce(_ ++ _)))
//  }
//
//}
//
///**
// * Factory methods for creating scalar images
// */
//object ScalarImage {
//
//  /**
//   *  Creates a new scalar image with given domain and values
//   *
//   * @param domain The domain over which the image is defined
//   * @param f A function which yields for each point of the domain its value
//   */
//  def apply[D: NDSpace](domain: Domain[D], f: Point[D] => Float) = new ScalarImage[D](domain, f)
//
//}
//
//
///**
// * Factory methods to create a differentiableScalarImage
// */
//object DifferentiableScalarImage {
//
//  /**
//   * creates a new differentiable image.
//   *
//   * @param domain the domain of the image
//   * @param f a function that yields the intensity for each point of the domain
//   * @param df the derivative of the function f
//   */
//  def apply[D: NDSpace](domain: Domain[D], f: Point[D] => Float, df: Point[D] => EuclideanVector[D]) = new DifferentiableScalarImage[D](domain, f, df)
//
//}
//
