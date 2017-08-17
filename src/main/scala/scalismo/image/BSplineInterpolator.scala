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

import breeze.linalg.DenseVector
import scalismo.common._
import scalismo.geometry.{IntVector, Point, _3D, Vector}
import scalismo.numerics.BSpline

case class BSplineInterpolator[A: Scalar](degree: Int) extends FieldInterpolator[_3D, DiscreteImageDomain[_3D], A, Float] {


  override def interpolate(discreteField: DiscreteField[_3D, DiscreteImageDomain[_3D], A]): Field[_3D, Float] = {

    val domain = discreteField.domain

    def doInterpolation(): DifferentiableScalarImage[_3D] = {
      val ck = determineCoefficients3D(degree, discreteField)
      val pointToIdx = domain.indexToPhysicalCoordinateTransform.inverse

      def iterateOnPoints(x: Point[_3D], splineBasis: ((Double, Double, Double) => Double)): Double = {

        val unitCoords = pointToIdx(x)
        val xUnit = unitCoords(0)
        val yUnit = unitCoords(1)
        val zUnit = unitCoords(2)

        val k1 = scala.math.ceil(xUnit - 0.5f * (degree + 1)).toInt
        val l1 = scala.math.ceil(yUnit - 0.5f * (degree + 1)).toInt
        val m1 = scala.math.ceil(zUnit - 0.5f * (degree + 1)).toInt

        val K = degree + 1

        var result = 0.0
        var k = k1
        var l = l1
        var m = m1

        while (m <= m1 + K - 1) {
          val mBC = DiscreteScalarImage.applyMirrorBoundaryCondition(m, domain.size(2))
          l = l1
          while (l <= l1 + K - 1) {
            val lBC = DiscreteScalarImage.applyMirrorBoundaryCondition(l, domain.size(1))
            k = k1
            while (k <= k1 + K - 1) {
              val kBC = DiscreteScalarImage.applyMirrorBoundaryCondition(k, domain.size(0))
              val pointId = domain.pointId(IntVector(kBC, lBC, mBC))
              result = result + ck(pointId.id) * splineBasis(xUnit - k, yUnit - l, zUnit - m)
              k = k + 1
            }
            l = l + 1
          }
          m = m + 1
        }
        result
      }

      val bSplineNthOrder = BSpline.nthOrderBSpline(degree) _
      val bSplineNmin1thOrder = BSpline.nthOrderBSpline(degree - 1) _

      def f(x: Point[_3D]) = {
        val splineBasis = (x: Double, y: Double, z: Double) => bSplineNthOrder(x) * bSplineNthOrder(y) * bSplineNthOrder(z)
        iterateOnPoints(x, splineBasis).toFloat
      }

      def df(x: Point[_3D]) = {
        val splineBasisD1 = (x: Double, y: Double, z: Double) => (bSplineNmin1thOrder(x + 0.5f) - bSplineNmin1thOrder(x - 0.5f)) * bSplineNthOrder(y) * bSplineNthOrder(z)
        val splineBasisD2 = (x: Double, y: Double, z: Double) => bSplineNthOrder(x) * (bSplineNmin1thOrder(y + 0.5f) - bSplineNmin1thOrder(y - 0.5f)) * bSplineNthOrder(z)
        val splineBasisD3 = (x: Double, y: Double, z: Double) => bSplineNthOrder(x) * bSplineNthOrder(y) * (bSplineNmin1thOrder(z + 0.5f) - bSplineNmin1thOrder(z - 0.5f))
        val dfx = (iterateOnPoints(x, splineBasisD1) * (1 / domain.spacing(0))).toFloat
        val dfy = (iterateOnPoints(x, splineBasisD2) * (1 / domain.spacing(1))).toFloat
        val dfz = (iterateOnPoints(x, splineBasisD3) * (1 / domain.spacing(2))).toFloat
        Vector(dfx, dfy, dfz)
      }

      val bbox = domain.boundingBox
      DifferentiableScalarImage(BoxDomain3D(bbox.origin, bbox.oppositeCorner), f, df)

    }

    def determineCoefficients3D[Pixel: Scalar](degree: Int, img: DiscreteField[_3D, DiscreteImageDomain[_3D], Pixel]): Array[Float] = {
      val numeric = implicitly[Scalar[Pixel]]
      val coeffs = DenseVector.zeros[Float](img.values.size)
      var z = 0
      var y = 0
      while (z < img.domain.size(2)) {
        y = 0
        while (y < img.domain.size(1)) {
          val rowValues = (0 until img.domain.size(0)).map(x => img(img.domain.pointId(IntVector(x, y, z))))

          // the c is an input-output argument here
          val c = rowValues.map(numeric.toFloat).toArray
          BSplineCoefficients.getSplineInterpolationCoefficients(degree, c)
          val idxInCoeffs = img.domain.pointId(IntVector(0, y, z)).id
          coeffs(idxInCoeffs until idxInCoeffs + img.domain.size(0)) := DenseVector(c)
          y = y + 1
        }
        z = z + 1
      }
      coeffs.data
    }

    doInterpolation()
  }

}



