package scalismo.common

trait Topology[D] {}

trait DiscreteDomain[D] {
  def topology: Topology[D]
  def pointSet: PointSet[D]
}
