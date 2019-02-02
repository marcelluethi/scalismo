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

import scalismo.common.{Domain, Field}
import scalismo.common.Field.Image
import scalismo.geometry.{Point, _1D, _2D, _3D}


object Image1D {
  def apply[A](domain : Domain[_1D], f : Point[_1D] => A) : Image[_1D, A] = {
    new Field(domain, f)
  }
}


object Image2D {
  def apply[A](domain : Domain[_2D], f : Point[_2D] => A) : Image[_2D, A] = {
    new Field(domain, f)
  }
}


object Image3D {
  def apply[A](domain : Domain[_3D], f : Point[_3D] => A) : Image[_3D, A] = {
    new Field(domain, f)
  }
}
