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

import scalismo.ScalismoTestSuite
import scalismo.common.{ BoxDomain1D, PointId }
import scalismo.common.interpolation.{ BSplineInterpolator1D, BSplineInterpolator2D, BSplineInterpolator3D }
import scalismo.geometry.{ IntVector1D, Point, Point1D, _1D }
import scalismo.io.ImageIO

class ResampleTests extends ScalismoTestSuite {

  describe("Resampling a 1D image") {

    val domain = DiscreteImageDomain1D(BoxDomain1D(Point1D(0), Point1D(1)), size = IntVector1D(10))
    val discreteImage = DiscreteImage1D[Short](domain, (p: Point[_1D]) => (100.0 * Math.sin(p.x * 10.0)).toShort)
    // here we do 1st order interpolation. 3rd order would not work, as it does not necessarily preserve the
    // pixel values at the strong edges - and we thus could not formulate a reasonable test
    val continuousImage = discreteImage.interpolate(BSplineInterpolator1D(0))

    it("yields the original discrete image") {
      val resampledImage = continuousImage.sample(discreteImage.domain, 0)
      discreteImage.values.size should equal(resampledImage.values.size)
      for (i <- 0 until discreteImage.values.size) {
        discreteImage(PointId(i)) should be(resampledImage(PointId(i)))
      }
    }
  }

  describe("Resampling a 2D image") {

    val testImgUrl = getClass.getResource("/lena.vtk").getPath
    val discreteImage = ImageIO.read2DScalarImage[Short](new File(testImgUrl)).get

    // here we do 1st order interpolation. 3rd order would not work, as it does not necessarily preserve the
    // pixel values at the strong edges - and we thus could not formulate a reasonable test
    val continuousImage = discreteImage.interpolate(BSplineInterpolator2D(0))

    it("yields the original discrete image") {
      val resampledImage = continuousImage.sample(discreteImage.domain, 0)
      discreteImage.values.size should equal(resampledImage.values.size)
      for (i <- 0 until discreteImage.values.size) {
        discreteImage(PointId(i)) should be(resampledImage(PointId(i)))
      }
    }
  }

  describe("Resampling a 3D image") {
    val path = getClass.getResource("/3dimage.nii").getPath
    val discreteImage = ImageIO.read3DScalarImage[Short](new File(path)).get
    val continuousImage = discreteImage.interpolate(BSplineInterpolator3D(0))

    it("yields the original discrete image") {
      val resampledImage = continuousImage.sample(discreteImage.domain, 0)
      for (i <- 0 until discreteImage.values.size by 100) {
        discreteImage(PointId(i)) should be(resampledImage(PointId(i)))
      }
    }
  }
}