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
package scalismo.kernels

import scalismo.common._
import scalismo.geometry.Point.implicits._
import scalismo.geometry._
import scalismo.numerics.{UniformSampler3D}
import scalismo.registration.Transformation
import scalismo.statisticalmodel.{GaussianProcess3D, LowRankGaussianProcess}
import scalismo.utils.Random
import scalismo.ScalismoTestSuite

class KernelTests extends ScalismoTestSuite {

  implicit val rng = Random(42L)

  describe("a Kernel") {
    it("yields correct multiple when  multiplied by a scalar") {
      val gk = GaussianKernel1D(3.5)
      val gkMult = gk * 100
      val pt1 = 0.1
      val pt2 = 1.0
      gk(pt1, pt2) * 100.0 should be(gkMult(pt1, pt2))
    }

    it("yields correct result when two kernels are added") {
      val gk = GaussianKernel1D(3.5)
      val gk2 = gk + gk
      val pt1 = 0.1
      val pt2 = 1.0
      gk(pt1, pt2) + gk(pt1, pt2) should be(gk2(pt1, pt2))
    }
  }
  describe("A scalar valued Gaussian kernel") {
    it("evaluated with twice the same argument yields 1") {
      val gk = GaussianKernel1D(3.5)
      gk(0.1, 0.1) should be(1.0 +- 1e-8)
    }

    it("given two arguments far apart yields almost 0") {
      val gk = GaussianKernel1D(1.0)
      gk(0.1, 100.0) should be(0.0 +- 1e-8)
    }
  }

  describe("A sample covariance kernel") {
    it("can reproduce the covariance function from random samples") {

      val domain = BoxDomain3D(Point3D(-5, 1, 3), Point3D(100, 90, 25))

      val samplerForNystromApprox = UniformSampler3D(domain, 7 * 7 * 7)

      val k = DiagonalKernel(GaussianKernel3D(100.0), 3)
      val mu = (pt: Point[_3D]) => EuclideanVector3D(1, 10, -5)
      val gp = LowRankGaussianProcess.approximateGPNystrom(
        GaussianProcess3D(Field3D(domain, mu), k),
        samplerForNystromApprox,
        500
      )

      val sampleTransformations = for (i <- (0 until 5000)) yield {
        // TODO: gp.sample() should (arguably) accept seed.
        val sample: (Point[_3D] => EuclideanVector[_3D]) = gp.sample()
        Transformation((x: Point[_3D]) => x + sample(x))

      }

      val testPtSampler = UniformSampler3D(domain, 1)
      val pts = testPtSampler.sample.map(_._1)

      val sampleCovKernel = SampleCovarianceKernel3D(sampleTransformations.toIndexedSeq, pts.size)

      // since mu always returns the same vector, it's enough to calculate it once
      val mux = mu(Point(0, 0, 0))
      for (x <- pts.par) {
        val mu2 = sampleCovKernel.mu(x)
        for (d <- 0 until 3) {
          mu2(d) should be(mux(d) +- 0.2)
        }
      }

      for (x <- pts.par; y <- pts) {
        val gpxy = gp.cov(x, y)
        val sampleCovxy = sampleCovKernel(x, y)
        for (d1 <- 0 until 3; d2 <- 0 until 3) {
          sampleCovxy(d1, d2) should be(gpxy(d1, d2) +- 0.1)
        }
      }
    }

  }

  describe("Two scalar valued kernels") {
    it("can be added and multiplied") {
      val k1 = GaussianKernel1D(1.0)
      val k2 = GaussianKernel1D(1.0)
      val ksum = k1 + k2
      val x = Point(0)
      val y = Point(1)
      ksum(x, y) should be(k1(x, y) + k2(x, y) +- 1e-5)

      val kprod = k1 * k2
      kprod(x, y) should be(k1(x, y) * k2(x, y) +- 1e-5)

      // test scalar multiplication
      (k1 * 2.0)(x, y) should be(k1(x, y) * 2.0 +- 1e-5)
    }
  }

  describe("Two matrix valued kernels") {
    it("can be added and multiplied") {
      val k1 = DiagonalKernel(GaussianKernel1D(1.0), 1)
      val k2 = DiagonalKernel(GaussianKernel1D(1.0), 1)
      val ksum = k1 + k2
      val x = Point(0.0)
      val y = Point(1.0)
      val ks = k1(x, y) + k2(x, y)
      ksum(x, y)(0, 0) should be(ks(0, 0) +- 1e-5)

      val kprod = k1 * k2
      val kp = k1(x, y) * k2(x, y)
      kprod(x, y)(0, 0) should be(kp(0, 0) +- 1e-5)

      // test scalar multiplication
      (k1 * 2.0)(x, y)(0, 0) should be(k1(x, y)(0, 0) * 2.0 +- 1e-5)
    }
  }

}