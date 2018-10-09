package cc.redberry.rings.sym

import cc.redberry.rings.scaladsl.{IntZ, Ring}
import cc.redberry.rings.scaladsl._

/** 3 x 3 determinants */
@SerialVersionUID(1L)
class Determinants3x3[E](cfRing: Ring[E]) extends Serializable {
  private val ring = MultivariateRing(cfRing, Array("p23", "p13", "p12"))
  private type Expr = ring.ElementType
  private type Rat = Rational[Expr]

  /// Compiled (fast) expressions
  lazy val obj_DerDet3x1 = ring("-2*p23*(p23-p12-p13)")
  lazy val obj_DerDet3x2 = ring("-2*p13*(p13-p12-p23)")
  lazy val obj_DerDet3x3 = ring("-2*p12*(p12-p13-p23)")
  lazy val obj_DerDet3x11 = ring("-4*p23")
  lazy val obj_DerDet3x12 = ring("-2*(p12-p23-p13)")
  lazy val obj_DerDet3x13 = ring("-2*(p13-p12-p23)")
  lazy val obj_DerDet3x21 = ring("-2*(p12-p23-p13)")
  lazy val obj_DerDet3x22 = ring("-4*p13")
  lazy val obj_DerDet3x23 = ring("-2*(p23-p12-p13)")
  lazy val obj_DerDet3x31 = ring("-2*(p13-p12-p23)")
  lazy val obj_DerDet3x32 = ring("-2*(p23-p12-p13)")
  lazy val obj_DerDet3x33 = ring("-4*p12")
  lazy val obj_DerDet3x0 = ring("-2*p12*p13*p23")
  lazy val obj_ge2 = ring("-4*p13*p23+2*p13^2-4*p12*p13+2*p23^2-4*p23*p12+2*p12^2")
  lazy val obj_all = Seq(obj_DerDet3x1, obj_DerDet3x2, obj_DerDet3x3, obj_DerDet3x11, obj_DerDet3x12, obj_DerDet3x13, obj_DerDet3x21, obj_DerDet3x22, obj_DerDet3x23, obj_DerDet3x31, obj_DerDet3x32, obj_DerDet3x33, obj_DerDet3x0, obj_ge2)

  /** evaluates compiled expression at given polynomial points */
  private def composition(expr: Expr, vals: Rational[Expr]*): Rational[Expr] = {
    assert(vals.forall(_.isIntegral))
    Rational(expr.composition(vals.map(_.numerator()): _*))(vals(0).ring)
  }

  def DerDet3x1(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_DerDet3x1, p23, p13, p12)

  def DerDet3x2(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_DerDet3x2, p23, p13, p12)

  def DerDet3x3(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_DerDet3x3, p23, p13, p12)

  def DerDet3x11(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_DerDet3x11, p23, p13, p12)

  def DerDet3x12(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_DerDet3x12, p23, p13, p12)

  def DerDet3x13(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_DerDet3x13, p23, p13, p12)

  def DerDet3x21(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_DerDet3x21, p23, p13, p12)

  def DerDet3x22(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_DerDet3x22, p23, p13, p12)

  def DerDet3x23(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_DerDet3x23, p23, p13, p12)

  def DerDet3x31(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_DerDet3x31, p23, p13, p12)

  def DerDet3x32(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_DerDet3x32, p23, p13, p12)

  def DerDet3x33(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_DerDet3x33, p23, p13, p12)

  def DerDet3x0(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_DerDet3x0, p23, p13, p12)

  def ge2(p23: Rat, p13: Rat, p12: Rat): Rat = composition(obj_ge2, p23, p13, p12)
}
