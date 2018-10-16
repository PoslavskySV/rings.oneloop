package cc.redberry.rings.legs

import cc.redberry.rings.scaladsl.{Ring, _}

/** 4 x 4 determinants */
class Determinants4x4[E](cfRing: Ring[E]) extends Serializable {
  private val ring = MultivariateRing(cfRing, Array("p12", "p23", "p34", "p14", "p24", "p13"))
  private type Expr = ring.ElementType
  private type Rat = Rational[Expr]

  /// Compiled (fast) expressions
  lazy val obj_DerDet4x1 = ring("-4*p23*p24*p34+2*p14*p34*p23+2*p14*p23*p24+2*p13*p24*p23+2*p13*p24*p34+2*p12*p34*p23+2*p34*p12*p24-2*p14*p23^2-2*p13*p24^2-2*p12*p34^2")
  lazy val obj_DerDet4x12 = ring("-4*p12*p34+2*p24*p34+2*p14*p34-2*p14*p24+2*p14*p23-2*p34^2+2*p13*p24-2*p13*p23+2*p34*p23+2*p34*p13")
  lazy val obj_DerDet4x13 = ring("2*p12*p24+2*p14*p24+2*p23*p24+2*p24*p34-2*p23*p12-2*p24^2-2*p14*p34-4*p13*p24+2*p14*p23+2*p12*p34")
  lazy val obj_DerDet4x14 = ring("-4*p14*p23+2*p12*p34+2*p23*p12+2*p34*p23+2*p13*p24-2*p12*p24-2*p34*p13+2*p23*p24-2*p23^2+2*p13*p23")
  lazy val obj_DerDet4x2 = ring("2*p14*p34*p23-4*p13*p14*p34+2*p13*p24*p14+2*p13*p14*p23+2*p13*p24*p34+2*p12*p13*p34+2*p34*p14*p12-2*p14^2*p23-2*p12*p34^2-2*p24*p13^2")
  lazy val obj_DerDet4x21 = ring("-4*p12*p34+2*p24*p34+2*p14*p34-2*p14*p24+2*p14*p23-2*p34^2+2*p13*p24-2*p13*p23+2*p34*p23+2*p34*p13")
  lazy val obj_DerDet4x23 = ring("-2*p12*p13+2*p14*p24+2*p14*p12+2*p13*p14 -2*p14^2-2*p24*p34+2*p14*p34+2*p13*p24-4*p14*p23+2*p12*p34")
  lazy val obj_DerDet4x24 = ring("2*p14*p23+2*p12*p34-2*p14*p12-2*p34*p23-4*p13*p24+2*p12*p13+2*p34*p13+2*p13*p14-2*p13^2+2*p13*p23")
  lazy val obj_DerDet4x3 = ring("-4*p12*p24*p14+2*p14*p23*p24+2*p13*p24*p14+2*p23*p12*p14+2*p12*p24*p13+2*p34*p14*p12+2*p34*p12*p24-2*p34*p12^2-2*p14^2*p23-2*p13*p24^2")
  lazy val obj_DerDet4x31 = ring("2*p12*p24+2*p14*p24+2*p23*p24+2*p24*p34-2*p23*p12-2*p24^2-2*p14*p34-4*p13*p24+2*p14*p23+2*p12*p34")
  lazy val obj_DerDet4x32 = ring("-2*p12*p13+2*p14*p24+2*p14*p12+2*p13*p14-2*p14^2-2*p24*p34+2*p14*p34+2*p13*p24-4*p14*p23+2*p12*p34")
  lazy val obj_DerDet4x34 = ring("2*p14*p23-4*p12*p34+2*p14*p12+2*p23*p12+2*p13*p24+2*p12*p13+2*p12*p24-2*p23*p24-2*p13*p14-2*p12^2")
  lazy val obj_DerDet4x4 = ring("2*p13*p14*p23+2*p13*p24*p23+2*p12*p34*p23+2*p12*p13*p34+2*p23*p12*p14-4*p12*p13*p23+2*p12*p24*p13-2*p14*p23^2-2*p34*p12^2-2*p24*p13^2")
  lazy val obj_DerDet4x41 = ring("-4*p14*p23+2*p12*p34+2*p23*p12+2*p34*p23+2*p13*p24-2*p12*p24-2*p34*p13+2*p23*p24-2*p23^2+2*p13*p23")
  lazy val obj_DerDet4x42 = ring("2*p14*p23+2*p12*p34-2*p14*p12-2*p34*p23-4*p13*p24+2*p12*p13+2*p34*p13+2*p13*p14-2*p13^2+2*p13*p23")
  lazy val obj_DerDet4x43 = ring("2*p14*p23-4*p12*p34+2*p14*p12+2*p23*p12+2*p13*p24+2*p12*p13+2*p12*p24-2*p23*p24-2*p13*p14-2*p12^2")
  lazy val obj_DerDet4x0 = ring("-2*p23*p12*p14*p34+p12^2*p34^2-2*p12*p24*p13*p34+p13^2*p24^2-2*p13*p24*p14*p23+p14^2*p23^2")
  lazy val obj_ge3 = ring("4*p23*p24*p34-4*p14*p34*p23+4*p13*p14*p34+4*p12*p24*p14-4*p14*p23*p24-4*p13*p24*p14-4*p13*p14*p23-4*p13*p24*p23-4*p13*p24*p34-4*p12*p34*p23-4*p12*p13*p34-4*p23*p12*p14+4*p12*p13*p23-4*p12*p24*p13-4*p34*p14*p12-4*p34*p12*p24+4*p14*p23^2+4*p34*p12^2+4*p14^2*p23+4*p13*p24^2+4*p12*p34^2+4*p24*p13^2")
  lazy val obj_all = Seq(obj_DerDet4x1, obj_DerDet4x12, obj_DerDet4x13, obj_DerDet4x14, obj_DerDet4x2, obj_DerDet4x21, obj_DerDet4x23, obj_DerDet4x24, obj_DerDet4x3, obj_DerDet4x31, obj_DerDet4x32, obj_DerDet4x34, obj_DerDet4x4, obj_DerDet4x41, obj_DerDet4x42, obj_DerDet4x43, obj_DerDet4x0, obj_ge3)

  /** evaluates compiled expression at given polynomial points */
  private def composition(expr: Expr, vals: Rational[Expr]*): Rational[Expr] = {
    assert(vals.forall(_.isIntegral))
    Rational(expr.composition(vals.map(_.numerator()): _*))(vals(0).ring)
  }

  def DerDet4x1(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x1, p12, p23, p34, p14, p24, p13)

  def DerDet4x12(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x12, p12, p23, p34, p14, p24, p13)

  def DerDet4x13(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x13, p12, p23, p34, p14, p24, p13)

  def DerDet4x14(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x14, p12, p23, p34, p14, p24, p13)

  def DerDet4x2(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x2, p12, p23, p34, p14, p24, p13)

  def DerDet4x21(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x21, p12, p23, p34, p14, p24, p13)

  def DerDet4x23(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x23, p12, p23, p34, p14, p24, p13)

  def DerDet4x24(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x24, p12, p23, p34, p14, p24, p13)

  def DerDet4x3(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x3, p12, p23, p34, p14, p24, p13)

  def DerDet4x31(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x31, p12, p23, p34, p14, p24, p13)

  def DerDet4x32(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x32, p12, p23, p34, p14, p24, p13)

  def DerDet4x34(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x34, p12, p23, p34, p14, p24, p13)

  def DerDet4x4(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x4, p12, p23, p34, p14, p24, p13)

  def DerDet4x41(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x41, p12, p23, p34, p14, p24, p13)

  def DerDet4x42(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x42, p12, p23, p34, p14, p24, p13)

  def DerDet4x43(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x43, p12, p23, p34, p14, p24, p13)

  def DerDet4x0(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_DerDet4x0, p12, p23, p34, p14, p24, p13)

  def ge3(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = composition(obj_ge3, p12, p23, p34, p14, p24, p13)

  lazy val det3_methods = new Determinants3x3[E](cfRing)

  import det3_methods._

  def DerDet4x11(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = ge2(p23, p34, p24)

  def DerDet4x22(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = ge2(p13, p14, p34)

  def DerDet4x33(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = ge2(p14, p24, p12)

  def DerDet4x44(p12: Rat, p23: Rat, p34: Rat, p14: Rat, p24: Rat, p13: Rat): Rat = ge2(p12, p13, p23)
}
