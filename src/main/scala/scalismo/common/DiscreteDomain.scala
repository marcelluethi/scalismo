package scalismo.common

import scalismo.geometry.{EuclideanVector, Point}
import scalismo.registration.Transformation

trait Topology[D] {}

trait DiscreteDomain[D] {
  def topology: Topology[D]
  def pointSet: PointSet[D]
}

trait DeformableDomain[D, DDomain <: DiscreteDomain[D]] extends DiscreteDomain[D] {
  def warp(warpField: DiscreteField[D, DiscreteDomain[D], EuclideanVector[D]]): DDomain
  def transform(transformation: Point[D] => Point[D]): DDomain
}
