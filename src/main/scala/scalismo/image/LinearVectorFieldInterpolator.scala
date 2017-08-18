package scalismo.image

import scalismo.common.{ Field, DiscreteField, FieldInterpolator }
import scalismo.geometry._
import scalismo.mesh.Interpolator$

/**
 * Created by luetma00 on 18.08.17.
 */
class LinearVectorFieldInterpolator[D <: Dim: NDSpace] extends FieldInterpolator[D, DiscreteImageDomain[D], Vector[D]] {

  override def interpolate(df: DiscreteField[D, DiscreteImageDomain[D], Vector[D]]): Field[D, Vector[D]] = {

    val domain = df.domain

    def V(d: Array[Double]) = {

      val i = IntVector[D](d.map(_.toInt))
      df(domain.pointId(i))

    }

    def pointToContinuousIndex(pt: Point[D]): Point[D] = {
      val dim = pt.dimensionality
      val data = (0 until dim).map(i => (pt(i) - domain.origin(i)) / domain.spacing(i))
      Point[D](data.toArray)
    }

    def isDefinedOnGrid(pt: Point[D]): Boolean = {
      val idx = pointToContinuousIndex(pt)
      def isInRange(i: Int) = idx(i) <= domain.size(i) - 1 && idx(i) >= 0
      (0 until pt.dimensionality).forall(i => isInRange(i))

    }

    val f = (p: Point[D]) => {
      if (!isDefinedOnGrid(p)) {
        //        apply(domain.findClosestPoint(p).id)
        Vector.zeros[D]
      } else {

        val ctdIndex = pointToContinuousIndex(p)

        p.dimensionality match {
          case 1 => {
            val x0 = ctdIndex(0).floor
            val x1 = if (x0 != ctdIndex(0)) x0 + 1.0 else x0
            val xd = (ctdIndex(0) - x0)
            val c = (x0 * (1.0f - xd)) + x1 * xd
            Vector[D](Array(c))
          }
          case 2 => {
            val x0 = ctdIndex(0).floor
            val x1 = if (x0 != ctdIndex(0)) x0 + 1.0 else x0
            val y0 = ctdIndex(1).floor
            val y1 = if (y0 != ctdIndex(1)) y0 + 1.0 else y0
            val xd = (ctdIndex(0) - x0)
            val yd = (ctdIndex(1) - y0)
            val c00 = (V(Array(x0, y0)) * (1.0f - xd)) + (V(Array(x1, y0)) * xd)
            val c10 = (V(Array(x0, y1)) * (1.0f - xd)) + (V(Array(x1, y1)) * xd)
            val c = (c00 * (1.0f - yd)) + c10 * yd
            c
          }
          case 3 => {

            val x0 = ctdIndex(0).floor
            val x1 = if (x0 != ctdIndex(0)) x0 + 1.0 else x0
            val y0 = ctdIndex(1).floor
            val y1 = if (y0 != ctdIndex(1)) y0 + 1.0 else y0
            val z0 = ctdIndex(2).floor
            val z1 = if (z0 != ctdIndex(2)) z0 + 1.0 else z0
            val xd = (ctdIndex(0) - x0)
            val yd = (ctdIndex(1) - y0)
            val zd = (ctdIndex(2) - z0)
            val c00 = (V(Array(x0, y0, z0)) * (1.0f - xd)) + (V(Array(x1, y0, z0)) * xd)
            val c01 = (V(Array(x0, y0, z1)) * (1.0f - xd)) + (V(Array(x1, y0, z1)) * xd)
            val c10 = (V(Array(x0, y1, z0)) * (1.0f - xd)) + (V(Array(x1, y1, z0)) * xd)
            val c11 = (V(Array(x0, y1, z1)) * (1.0f - xd)) + (V(Array(x1, y1, z1)) * xd)
            val c0 = (c00 * (1.0f - yd)) + (c10 * yd)
            val c1 = (c01 * (1.0f - yd)) + (c11 * yd)
            val c = (c0 * (1.0f - zd)) + c1 * zd
            c
          }
          case _ => ???
        }

      }

    }
    Field[D, Vector[D]](domain.boundingBox, f)
  }
}
