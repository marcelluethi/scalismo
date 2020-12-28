/*
 * Copyright 2016 University of Basel, Graphics and Vision Research Group
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
package scalismo.sampling

/** a probability or density */
trait DistributionEvaluator[A] {

  /** log probability/density of sample */
  def logValue(sample: A): Double
}

/** gradient evaluator, used together with DistributionEvaluator */
trait GradientEvaluator[A] {

  /** gradient (of log density) at sample */
  def gradient(sample: A): A
}
