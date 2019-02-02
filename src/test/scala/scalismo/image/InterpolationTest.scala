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
package scalismo.image

import java.io.File

import org.scalatest.PrivateMethodTester
import scalismo.ScalismoTestSuite
import scalismo.common.interpolation.{ BSplineInterpolator1D, BSplineInterpolator2D, BSplineInterpolator3D }
import scalismo.common._
import scalismo.geometry.IntVector.implicits._
import scalismo.geometry.Point.implicits._
import scalismo.geometry.EuclideanVector.implicits._
import scalismo.geometry._
import scalismo.io.ImageIO

import scala.language.implicitConversions

class InterpolationTest extends ScalismoTestSuite with PrivateMethodTester {

  implicit def doubleToFloat(d: Double): Float = d.toFloat

  describe("A 1D Interpolation with 0rd order bspline") {

    it("interpolates the values for origin 2.3 and spacing 1.5") {
      val domain = DiscreteImageDomain1D(2.3, 1.5, 7)
      val discreteImage = DiscreteImage1D(domain, IndexedSeq[Float](1.4, 2.1, 7.5, 9.0, 8.0, 0.0, 2.1))
      val continuousImg = discreteImage.interpolate(NearestNeighborInterpolator1D())
      for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
        continuousImg(pt) should be(discreteImage(PointId(idx)) +- 0.0001f)
      }
    }
  }

  describe("A 1D Interpolation with 1th order bspline") {

    it("interpolates the values for origin 2.3 and spacing 1.5") {
      val domain = DiscreteImageDomain1D(2.3, 1.5, 7)
      val discreteImage = DiscreteImage1D[Float](domain, IndexedSeq[Float](1.4, 2.1, 7.5, 9, 8, 0, 2.1))
      val continuousImg = discreteImage.interpolate(BSplineInterpolator1D(1))
      for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
        continuousImg(pt) should be(discreteImage(PointId(idx)) +- 0.0001f)
      }
    }

    it("interpolates the values for origin 0 and spacing 1") {
      val domain = DiscreteImageDomain[_1D](0.0, 1.0, 5)
      val discreteImage = DiscreteImage1D(domain, IndexedSeq(3.0, 2.0, 1.5, 1.0, 0.0))
      val continuousImg = discreteImage.interpolate(NearestNeighborInterpolator1D())
      for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
        assert(continuousImg(pt) === discreteImage(PointId(idx)))
      }
    }

    describe("A 1D Interpolation with 3rd order bspline") {

      it("Derivative of interpolated Sine function is the Cosine") {
        val domain = DiscreteImageDomain1D(-2.0, 0.01, 400)

        val discreteSinImage = DiscreteImage1D(domain, domain.points.map(x => math.sin(x * math.Pi)).toIndexedSeq)
        val interpolatedSinImage = BSplineInterpolator1D[Double](3).interpolate(discreteSinImage)
        val derivativeImage = interpolatedSinImage.differentiate

        val discreteCosImage = DiscreteImage1D(domain, domain.points.map(x => math.Pi * math.cos(x * math.Pi)).toIndexedSeq)

        for ((pt, idx) <- domain.points.zipWithIndex.filter(x => math.abs(x._1) < 1.90)) {
          derivativeImage(pt)(0).toDouble should be(discreteCosImage(PointId(idx)) +- 0.0001f)
        }
      }
    }
  }

  describe("A 2D interpolation  Spline") {

    describe("of degree 0") {

      it("Interpolates the values for a simple domain") {
        val domain = DiscreteImageDomain[_2D]((0.0, 0.0), (1.0, 1.0), (2, 3))
        val discreteImage = DiscreteImage2D(domain, IndexedSeq(1f, 2f, 3f, 4f, 5f, 6f))

        val continuousImg = discreteImage.interpolate(BSplineInterpolator2D(0))

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt) should be(discreteImage(PointId(idx)) +- 0.0001f)
        }
      }

      it("Interpolates the values for origin (2,3) and spacing (1.5, 2.3)") {
        val domain = DiscreteImageDomain[_2D]((2.0, 3.0), (1.5, 0.1), (2, 3))
        val discreteImage = DiscreteImage2D(domain, IndexedSeq(1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

        val continuousImg = discreteImage.interpolate(interpolation.BSplineInterpolator2D(2))

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt) should be(discreteImage(PointId(idx)) +- 0.0001f)
        }
      }
    }
    describe(" of degree 3") {
      it("Interpolates the values for origin (2,3) and spacing (1.5, 2.3)") {
        val domain = DiscreteImageDomain[_2D]((2.0, 3.0), (1.5, 1.3), (10, 10))
        val discreteImage = DiscreteImage2D(domain, domain.points.map(x => x(0).toFloat).toArray)

        val continuousImg = discreteImage.interpolate(BSplineInterpolator2D(3))

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt) should be(discreteImage(PointId(idx)) +- 0.0001f)
        }
      }

      it("Interpolates the values correctly for a test dataset") {
        val testImgUrl = getClass.getResource("/lena256.vtk").getPath
        val discreteFixedImage = ImageIO.read2DScalarImage[Short](new File(testImgUrl)).get
        val interpolatedImage = discreteFixedImage.interpolate(BSplineInterpolator2D(2))

        for ((p, i) <- discreteFixedImage.domain.points.zipWithIndex) {
          interpolatedImage(p).toShort should be(discreteFixedImage(PointId(i)) +- 30)
        }
      }

      it("Derivative of interpolated function is correct") {
        val domain = DiscreteImageDomain2D((-2.0, -2.0), (0.01, 0.01), (400, 400))

        val discreteFImage = DiscreteImage2D(domain, domain.points.map(x => x(0) * x(0) + x(1) * x(1)).toArray)
        val interpolatedFImage = BSplineInterpolator2D[Double](3).interpolate(discreteFImage)
        val derivativeImage = interpolatedFImage.differentiate

        for ((pt, idx) <- domain.points.zipWithIndex.filter(x => math.abs(x._1(0)) < 1.90 && math.abs(x._1(1)) < 1.90)) {
          derivativeImage(pt)(0) should be((2 * pt(0)) +- 0.001f)
          derivativeImage(pt)(1) should be((2 * pt(1)) +- 0.001f)
        }
      }
    }
  }
  describe("A 3D interpolation  Spline") {
    describe("of degree 0") {

      it("Interpolates the values for origin (2,3,0) and spacing (1.5, 1.3, 2)") {
        val domain = DiscreteImageDomain3D((2.0, 3.0, 0.0), (1.5, 1.3, 2.0), (2, 3, 2))
        val discreteImage = DiscreteImage3D(domain, Array(1.4f, 2.1f, 7.5f, 9f, 8f, 0f, 1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

        val continuousImg = discreteImage.interpolate(BSplineInterpolator3D(0))

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt) should be(discreteImage(PointId(idx)) +- 0.0001f)
        }
      }
    }

    describe(" of degree 1") {
      it("Interpolates the values for origin (2,3,0) and spacing (1.5, 1.3, 2)") {
        val domain = DiscreteImageDomain3D((2.0, 3.0, 0.0), (1.5, 1.3, 2.0), (2, 3, 2))
        val discreteImage = DiscreteImage3D[Float](domain, Array(1.4f, 2.1f, 7.5f, 9f, 8f, 0f, 1.4f, 2.1f, 7.5f, 9f, 8f, 0f))

        val continuousImg = discreteImage.interpolate(BSplineInterpolator3D(1))

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt) should be(discreteImage(PointId(idx)) +- 0.0001f)
        }
      }
    }

    describe(" of degree 3") {
      it("Interpolates the values for origin (2,3,0) and spacing (1.5, 1.3, 2)") {
        val domain = DiscreteImageDomain3D((2.0, 3.0, 0.0), (1.5, 1.3, 2.0), (10, 10, 10))
        val discreteImage = DiscreteImage3D[Float](domain, domain.points.map(x => x(0).toFloat).toArray)

        val continuousImg = discreteImage.interpolate(BSplineInterpolator3D(3))

        for ((pt, idx) <- discreteImage.domain.points.zipWithIndex) {
          continuousImg(pt) should be(discreteImage(PointId(idx)) +- 0.0001f)
        }
      }

      it("Derivative of interpolated function is correct") {
        val domain = DiscreteImageDomain3D((-2.0, -2.0, -2.0), (0.1, 0.1, 0.1), (40, 40, 40))

        val discreteFImage = DiscreteImage3D(domain, domain.points.map(x => x(0) * x(0) + x(1) * x(1) + x(2) * x(2)).toArray)
        val interpolatedFImage = BSplineInterpolator3D[Double](3).interpolate(discreteFImage)
        val derivativeImage = interpolatedFImage.differentiate

        for ((pt, idx) <- domain.points.zipWithIndex.filter(x => math.abs(x._1(0)) < 1.0 && math.abs(x._1(1)) < 1.0 && math.abs(x._1(2)) < 1.0)) {
          derivativeImage(pt)(0) should be((2 * pt(0)) +- 0.0001)
          derivativeImage(pt)(1) should be((2 * pt(1)) +- 0.0001)
          derivativeImage(pt)(2) should be((2 * pt(2)) +- 0.0001)
        }
      }

      it("Interpolates a real dataset correctly") {
        val path = getClass.getResource("/3dimage.nii").getPath
        val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get
        val continuousImage = discreteImage.interpolate(BSplineInterpolator3D(3))

        for ((p, i) <- discreteImage.domain.points.zipWithIndex.filter(p => p._2 % 100 == 0))
          discreteImage(PointId(i)) should be(continuousImage(p).toShort +- 1.toShort)
      }
    }
  }
}
