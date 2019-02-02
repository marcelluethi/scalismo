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

import scalismo.common.DiscreteField
import scalismo.geometry._

object DiscreteImage1D {

  def apply[A](domain : DiscreteImageDomain[_1D], values: IndexedSeq[A])
  : DiscreteField[_1D, DiscreteImageDomain[_1D], A] = {
    new DiscreteField(domain, values)
  }

  def apply[A](domain : DiscreteImageDomain[_1D], f: Point[_1D] => A)
  : DiscreteField[_1D, DiscreteImageDomain[_1D], A] = {

    val data = domain.points.map(f).toIndexedSeq
    new DiscreteField(domain, data)
  }

}

object DiscreteImage2D {
  def apply[A](domain : DiscreteImageDomain[_2D], values: IndexedSeq[A])
  : DiscreteField[_2D, DiscreteImageDomain[_2D], A] = {
    new DiscreteField(domain, values)
  }

  def apply[A](domain : DiscreteImageDomain[_2D], f: Point[_2D] => A)
  : DiscreteField[_2D, DiscreteImageDomain[_2D], A] = {

    val data = domain.points.map(f).toIndexedSeq
    new DiscreteField(domain, data)
  }
}


object DiscreteImage3D {
  def apply[A](domain : DiscreteImageDomain[_3D], values: IndexedSeq[A])
  : DiscreteField[_3D, DiscreteImageDomain[_3D], A] = {
    new DiscreteField(domain, values)
  }

  def apply[A](domain : DiscreteImageDomain[_3D], f: Point[_3D] => A)
  : DiscreteField[_3D, DiscreteImageDomain[_3D], A] = {

    val data = domain.points.map(f).toIndexedSeq
    new DiscreteField(domain, data)
  }
}
