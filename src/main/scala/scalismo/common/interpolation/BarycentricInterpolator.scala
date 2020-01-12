package scalismo.common.interpolation

import scalismo.common._
import scalismo.geometry.{_3D, NDSpace, Point, Point3D}
import scalismo.mesh.{TetrahedralCell, TetrahedralMesh, TetrahedronId}
import scalismo.numerics.ValueInterpolator
import scalismo.utils.Memoize

trait BarycentricInterpolator[D, A] extends FieldInterpolator[D, TetrahedralMesh, A] {
  implicit protected val valueInterpolator: ValueInterpolator[A]
}

object BarycentricInterpolator {

  trait Create[D] {
    def createBarycentricInterpolator[A: ValueInterpolator](): BarycentricInterpolator[D, A]
  }

  implicit object create3D extends Create[_3D] {
    override def createBarycentricInterpolator[A: ValueInterpolator](): BarycentricInterpolator[_3D, A] =
      new BarycentricInterpolator3D[A]()
  }

  def apply[D: NDSpace, A: ValueInterpolator]()(implicit creator: Create[D]): BarycentricInterpolator[D, A] = {
    creator.createBarycentricInterpolator()
  }
}

case class BarycentricInterpolator3D[A: ValueInterpolator]() extends BarycentricInterpolator[_3D, A] {

  override protected val valueInterpolator: ValueInterpolator[A] = ValueInterpolator[A]

  // TODO: Temporary solution, replace for Milestone M2!
  private def getTetrahedralMeshCell(mesh: TetrahedralMesh[_3D], p: Point[_3D]): Option[TetrahedralCell] = {

    val numberOfTetrahedrons = mesh.tetrahedralization.tetrahedrons.length

    def isInsideCell(tc: TetrahedralCell): Boolean = mesh.isInsideTetrahedralCell(p, tc)
    val isInsideCellMemoized = Memoize(isInsideCell, numberOfTetrahedrons)

    var cell: Option[TetrahedralCell] = None
    var neighbourhood = Set[TetrahedronId]()

    while (cell.isEmpty && neighbourhood.size != numberOfTetrahedrons) {
      if (neighbourhood.isEmpty) {
        // start from closest vertex point
        val closestPoint = mesh.pointSet.findClosestPoint(p).id
        neighbourhood = mesh.tetrahedralization.adjacentTetrahedronsForPoint(closestPoint).toSet
      } else {
        // increase neighbourhood
        neighbourhood =
          neighbourhood.union(neighbourhood.flatMap(mesh.tetrahedralization.adjacentTetrahedronsForTetrahedron))
      }
      val filterResult = neighbourhood.filter(tId => isInsideCellMemoized(mesh.tetrahedralization.tetrahedrons(tId.id)))
      if (filterResult.nonEmpty) cell = Some(mesh.tetrahedralization.tetrahedrons(filterResult.head.id))
    }
    cell
  }

  override def interpolate(df: DiscreteField[_3D, TetrahedralMesh, A]): Field[_3D, A] = {

    val mesh = df.domain

    def interpolateBarycentric(p: Point[_3D]): A = {
      getTetrahedralMeshCell(mesh, p) match {
        case Some(cell) =>
          val vertexValues = cell.pointIds.map(df(_))
          val barycentricCoordinates = mesh.getBarycentricCoordinates(p, cell)
          val valueCoordinatePairs = vertexValues.zip(barycentricCoordinates)
          ValueInterpolator[A].convexCombination(valueCoordinatePairs(0),
                                                 valueCoordinatePairs(1),
                                                 valueCoordinatePairs(2),
                                                 valueCoordinatePairs(3))
        case None => throw new Exception(s"Point $p outside of domain.")
      }
    }
    Field(RealSpace[_3D], interpolateBarycentric)
  }
}
