package scalismo.transformations

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.{Domain, EuclideanSpace, Field, RealSpace}
import scalismo.geometry.{_1D, _2D, _3D, Point, SquareMatrix}
import scalismo.transformations.ParametricTransformation.JacobianField
import scalismo.utils.Memoize

/**
 * Trait for D-dimensional transformations that maps a D-dimensional Point to another.
 *  A transformations in our library is seen as a particular type of Field (or image)  mapping points
 *  to values that are also of type [[scalismo.geometry.Point]]
 */
trait Transformation[D] extends Field[D, Point[D]] {}

/** Trait for parametric D-dimensional transformations */
object Transformation {

  /**
   * Create a transformations defined on the whole real space with the given function
   */
  def apply[D](t: Point[D] => Point[D]): Transformation[D] = {
    new Transformation[D] {
      override val domain = EuclideanSpace[D]
      override val f = t
    }
  }

  def apply[D](domain: Domain[D], t: Point[D] => Point[D]): Transformation[D] = {
    val outerDom = domain;
    new Transformation[D] {
      override val domain = outerDom
      override val f = t
    }
  }

  /**
   * Returns a new transformations that memoizes (caches) the values that have already been
   * computed. The size of the cache used is given by the argument cacheSizeHint.
   */
  def memoize[D](t: Transformation[D], cacheSizeHint: Int) = {
    val f: (Point[D]) => Point[D] = Memoize(t.f, cacheSizeHint)
    val domain: Domain[D] = t.domain
    Transformation(domain, f)
  }

}

object Transformation1D {
  def apply(t: Point[_1D] => Point[_1D]): Transformation[_1D] = {
    Transformation(t)
  }
  def apply(domain: Domain[_1D], t: Point[_1D] => Point[_1D]): Transformation[_1D] = {
    Transformation(domain, t)
  }
}

object Transformation2D {
  def apply(t: Point[_2D] => Point[_2D]): Transformation[_2D] = {
    Transformation(t)
  }
  def apply(domain: Domain[_2D], t: Point[_2D] => Point[_2D]): Transformation[_2D] = {
    Transformation(domain, t)
  }
}

object Transformation3D {
  def apply(t: Point[_3D] => Point[_3D]): Transformation[_3D] = {
    Transformation(t)
  }
  def apply(domain: Domain[_3D], t: Point[_3D] => Point[_3D]): Transformation[_3D] = {
    Transformation(domain, t)
  }
}

trait ParametricTransformation[D] extends Transformation[D] {
  def parameters: DenseVector[Double]

  def numberOfParameters: Int

  def jacobian: JacobianField[D]

}
object ParametricTransformation {
  type JacobianField[D] = Field[D, DenseMatrix[Double]]
}

/** Trait for invertible D-dimensional transformations */
trait CanInvert[D] {
  self: ParametricTransformation[D] =>

  def inverse: ParametricTransformation[D] with CanInvert[D]
}

/** Trait for differentiable D-dimensional transformations */
trait CanDifferentiate[D] {
  self: ParametricTransformation[D] =>

  /** Derivative of the transform evaluated at a point */
  def derivative: Point[D] => SquareMatrix[D]

  @deprecated("please use derivative instead", "v0.19")
  def takeDerivative(p: Point[D]): SquareMatrix[D] = derivative(p)

}