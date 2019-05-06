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

import scalismo.common.interpolation.FieldInterpolator
import scalismo.geometry._

/**
 * Nearest neighbor interpolation of a discrete field. This implementation is generic and
 * works for any discrete field.
 */
class NearestNeighborInterpolator[D: NDSpace, A]() extends FieldInterpolator[D, DiscreteDomain[D], A] {

  override def interpolate(df: DiscreteField[D, DiscreteDomain[D], A]): Field[D, A] = {

    def valueAtClosestPoint(p: Point[D]): A = {
      val closestPointId = df.domain.findClosestPoint(p).id
      df(closestPointId)
    }

    Field(RealSpace[D], valueAtClosestPoint)
  }

}

object NearestNeighborInterpolator1D {
  def apply[A](): NearestNeighborInterpolator[_1D, A] = {
    new NearestNeighborInterpolator[_1D, A]()
  }
}

object NearestNeighborInterpolator2D {
  def apply[A](): NearestNeighborInterpolator[_2D, A] = {
    new NearestNeighborInterpolator[_2D, A]()
  }
}

object NearestNeighborInterpolator3D {
  def apply[A](): NearestNeighborInterpolator[_3D, A] = {
    new NearestNeighborInterpolator[_3D, A]()
  }
}