package scalismo.common

import scalismo.geometry.{EuclideanVector, Point}
import scalismo.registration.Transformation

trait Topology[D] {}

trait DiscreteDomain[D] {
  def pointSet: PointSet[D]
}

trait CanWarp[D, DDomain <: DiscreteDomain[D]] {

  /**
   * Warp the points of the domain of the discrete field and turn it into the
   * warped domain
   */
  def warpDomain(warpField: DiscreteField[D, DDomain, EuclideanVector[D]]): DDomain
  def transform(pointSet: DDomain, transformation: Point[D] => Point[D]): DDomain
}
