package cc.redberry.rings.sym

import cc.redberry.rings.sym.commons.cfRing

import scala.collection.immutable.Seq

/** 5-point function */
object fivepoint {

  import Functions._
  import commons._
  import cc.redberry.rings.scaladsl._
  import cc.redberry.rings.scaladsl.syntax._
  import scala.collection.mutable

  /** The underlying auxiliary ring: polynomials in 10 kinematical invariants + space-time dimension */
  val pRing = MultivariateRing(cfRing, Array("d", "p12", "p23", "p34", "p45", "p15", "p13", "p14", "p24", "p25", "p35"))
  /** Rational functions */
  implicit val ring = Frac(pRing)
  type Poly = pRing.ElementType
  type Rat = ring.ElementType
  type Ret = SymFuncSum[Rat]

  import Det3._
  import Det4._
  import Det5._

  /** auxiliary method used to shuffle variables (from <-> to) */
  private[fivepoint]
  def replaceVariables0(expr: Poly, from: Seq[Poly], to: Seq[Poly]): Poly = {
    val varsFrom = from.map(_.univariateVariable()).toArray
    val varsTo = to.map(_.univariateVariable()).toArray
    assert(expr.degrees().zipWithIndex.forall { case (d, i) => varsFrom.contains(i) || d == 0 })

    expr.mapTerms(expr.ring, term => {
      val oldExponents = term.exponents
      val newExponents = Array.ofDim[Int](oldExponents.length)

      (varsFrom zip varsTo).foreach { case (f, t) =>
        newExponents(t) = oldExponents(f)
      }

      term.setDegreeVector(newExponents, term.totalDegree)
    })
  }

  /** auxiliary method used to shuffle variables (from <-> to) */
  private[fivepoint]
  def replaceVariables(expr: Rat, from: Seq[Rat], to: Seq[Rat]): Rat = {
    val _from = from.map(_.numerator())
    val _to = to.map(_.numerator())
    expr.map((f: Poly) => replaceVariables0(f, _from, _to))
  }

  /** some simple checks */
  private[fivepoint]
  def assertInput(seq: Rat*): Unit = assert(seq.forall(r => r.isIntegral && r.numerator().isVariable))

  /** logging */
  private[fivepoint]
  def log(msg: String): Unit = println(msg)

  /**
    * cached expression *
    *
    * @param lhs         left hand side
    * @param cachedValue r.h.s
    * @param vars        initial arguments of l.h.s.
    */
  @SerialVersionUID(1L)
  case class CachedExpression(lhs: Ret, cachedValue: Ret, vars: Seq[Rat]) extends Serializable {
    def apply(newVars: Seq[Rat]): Ret = {
      log("Using cached value of " + lhs.map(r => replaceVariables(r, vars, newVars)).stringify)
      cachedValue.map(r => replaceVariables(r, vars, newVars))
    }
  }

  @SerialVersionUID(1L)
  case class GlobalCache(cachedI5: mutable.Map[(Int, Int, Int, Int, Int, Rat), CachedExpression],
                         cachedI4: mutable.Map[(Int, Int, Int, Int, Rat), CachedExpression],
                         cachedI3: mutable.Map[(Int, Int, Int, Rat), CachedExpression])
    extends Serializable

  /** Cache of 5-point integrals */
  val cachedI5: mutable.Map[(Int, Int, Int, Int, Int, Rat), CachedExpression] = mutable.Map.empty
  /** Cache of 4-point integrals */
  val cachedI4: mutable.Map[(Int, Int, Int, Int, Rat), CachedExpression] = mutable.Map.empty
  /** Cache of 3-point integrals */
  val cachedI3: mutable.Map[(Int, Int, Int, Rat), CachedExpression] = mutable.Map.empty

  /** Computes 5-point massless integral, first looking into a cache */
  def I5msls(n1: Int, n2: Int, n3: Int, n4: Int, n5: Int, d: Rat,
             s12: Rat, s23: Rat, s34: Rat, s45: Rat, s15: Rat,
             s13: Rat, s14: Rat, s24: Rat, s25: Rat, s35: Rat): Ret = {

    val cacheKey = (n1, n2, n3, n4, n5, d)
    cachedI5.get(cacheKey) match {
      case Some(cached) => cached(Seq(d, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))
      case None =>
        val integral = __I5msls(n1, n2, n3, n4, n5, d, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
        cachedI5 += (cacheKey -> CachedExpression(
          "I5msls".func(ring(n1), ring(n2), ring(n3), ring(n4), ring(n5), d, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35),
          integral,
          Seq(d, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)))
        integral
    }
  }

  /** actual calculation of 5-point massless integral */
  private[fivepoint]
  def __I5msls(n1: Int, n2: Int, n3: Int, n4: Int, n5: Int, d: Rat,
               s12: Rat, s23: Rat, s34: Rat, s45: Rat, s15: Rat,
               s13: Rat, s14: Rat, s24: Rat, s25: Rat, s35: Rat): Ret = {

    assertInput(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)

    if (n1 == 0)
      return II4msls(n2, n3, n4, n5, d, s23, s34, s45, s25, s35, s24)

    if (n2 == 0)
      return II4msls(n1, n3, n4, n5, d, s13, s34, s45, s15, s35, s14)

    if (n3 == 0)
      return II4msls(n1, n2, n4, n5, d, s12, s24, s45, s15, s25, s14)

    if (n4 == 0)
      return II4msls(n1, n2, n3, n5, d, s12, s23, s35, s15, s25, s13)

    if (n5 == 0)
      return II4msls(n1, n2, n3, n4, d, s12, s23, s34, s14, s24, s13)

    if (n1 > 1)
      return ring(1) /
        ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n1 - 1) *
        (DerDet5x1(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1 - 1, n2, n3, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x11(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1 - 2, n2, n3, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x12(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1 - 1, n2 - 1, n3, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x13(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1 - 1, n2, n3 - 1, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x14(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1 - 1, n2, n3, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x15(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1 - 1, n2, n3, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if (n2 > 1)
      return ring(1) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n2 - 1) *
        (DerDet5x2(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2 - 1, n3, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x21(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1 - 1, n2 - 1, n3, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x22(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2 - 2, n3, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x23(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2 - 1, n3 - 1, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x24(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2 - 1, n3, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x25(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2 - 1, n3, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if (n3 > 1)
      return ring(1) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n3 - 1) *
        (DerDet5x3(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2, n3 - 1, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x31(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1 - 1, n2, n3 - 1, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x32(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2 - 1, n3 - 1, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x33(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2, n3 - 2, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x34(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2, n3 - 1, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x35(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2, n3 - 1, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if (n4 > 1)
      return ring(1) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n4 - 1) *
        (DerDet5x4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2, n3, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x41(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1 - 1, n2, n3, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x42(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2 - 1, n3, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x43(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2, n3 - 1, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x44(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2, n3, n4 - 2, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x45(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2, n3, n4 - 1, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if (n5 > 1)
      return ring(1) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n5 - 1) *
        (DerDet5x5(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2, n3, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x51(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1 - 1, n2, n3, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x52(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2 - 1, n3, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x53(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2, n3 - 1, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x54(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2, n3, n4 - 1, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x55(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(n1, n2, n3, n4, n5 - 2, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if ((n1, n2, n3, n4, n5) == (1, 1, 1, 1, 1)) {
      val b = ring(d.numerator().cc())
      if (b.signum() < 0)
        return ring(1) / 2 / DerDet5x0(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) *
          (ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * (d - 4) * I5msls(1, 1, 1, 1, 1, d + 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
            - DerDet5x1(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * II4msls(1, 1, 1, 1, d, s23, s34, s45, s25, s35, s24)
            - DerDet5x2(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * II4msls(1, 1, 1, 1, d, s13, s34, s45, s15, s35, s14)
            - DerDet5x3(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * II4msls(1, 1, 1, 1, d, s12, s24, s45, s15, s25, s14)
            - DerDet5x4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * II4msls(1, 1, 1, 1, d, s12, s23, s35, s15, s25, s13)
            - DerDet5x5(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * II4msls(1, 1, 1, 1, d, s12, s23, s34, s14, s24, s13))

      if (b.signum() > 0)
        return ring(1) / (d - 6) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) *
          (DerDet5x0(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * I5msls(1, 1, 1, 1, 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * 2
            + DerDet5x1(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * II4msls(1, 1, 1, 1, d - 2, s23, s34, s45, s25, s35, s24)
            + DerDet5x2(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * II4msls(1, 1, 1, 1, d - 2, s13, s34, s45, s15, s35, s14)
            + DerDet5x3(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * II4msls(1, 1, 1, 1, d - 2, s12, s24, s45, s15, s25, s14)
            + DerDet5x4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * II4msls(1, 1, 1, 1, d - 2, s12, s23, s35, s15, s25, s13)
            + DerDet5x5(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * II4msls(1, 1, 1, 1, d - 2, s12, s23, s34, s14, s24, s13))
    }

    return "I5msls".func(ring(n1), ring(n2), ring(n3), ring(n4), ring(n5), d, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
  }

  /** Computes 4-point massless integral, first looking into a cache */
  def I4msls(n1: Int, n2: Int, n3: Int, n4: Int, d: Rat,
             s12: Rat, s23: Rat, s34: Rat, s14: Rat, s24: Rat, s13: Rat): Ret = {

    val cacheKey = (n1, n2, n3, n4, d)
    cachedI4.get(cacheKey) match {
      case Some(cached) => cached(Seq(d, s12, s23, s34, s14, s24, s13))
      case None =>
        val integral = __I4msls(n1, n2, n3, n4, d, s12, s23, s34, s14, s24, s13)
        cachedI4 += (cacheKey -> CachedExpression(
          "I4msls".func(ring(n1), ring(n2), ring(n3), ring(n4), d, s12, s23, s34, s14, s24, s13),
          integral,
          Seq(d, s12, s23, s34, s14, s24, s13)))
        integral
    }
  }

  /** actual calculation of 4-point massless integral */
  private[fivepoint]
  def __I4msls(n1: Int, n2: Int, n3: Int, n4: Int, d: Rat,
               s12: Rat, s23: Rat, s34: Rat, s14: Rat, s24: Rat, s13: Rat): Ret = {

    assertInput(s12, s23, s34, s14, s24, s34, s14, s24, s13)

    if (n1 == 0)
      return II3msls(n2, n3, n4, d, s34, s24, s23)

    if (n2 == 0)
      return II3msls(n1, n3, n4, d, s34, s14, s13)

    if (n3 == 0)
      return II3msls(n1, n2, n4, d, s24, s14, s12)

    if (n4 == 0)
      return II3msls(n1, n2, n3, d, s23, s13, s12)


    if (n1 > 1)
      return ring(1) / ge3(s12, s23, s34, s14, s24, s13) / (n1 - 1) *
        (DerDet4x1(s12, s23, s34, s14, s24, s13) * I4msls(n1 - 1, n2, n3, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x11(s12, s23, s34, s14, s24, s13) * I4msls(n1 - 2, n2, n3, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x12(s12, s23, s34, s14, s24, s13) * I4msls(n1 - 1, n2 - 1, n3, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x13(s12, s23, s34, s14, s24, s13) * I4msls(n1 - 1, n2, n3 - 1, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x14(s12, s23, s34, s14, s24, s13) * I4msls(n1 - 1, n2, n3, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13))

    if (n2 > 1)
      return ring(1) / ge3(s12, s23, s34, s14, s24, s13) / (n2 - 1) *
        (DerDet4x2(s12, s23, s34, s14, s24, s13) * I4msls(n1, n2 - 1, n3, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x21(s12, s23, s34, s14, s24, s13) * I4msls(n1 - 1, n2 - 1, n3, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x22(s12, s23, s34, s14, s24, s13) * I4msls(n1, n2 - 2, n3, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x23(s12, s23, s34, s14, s24, s13) * I4msls(n1, n2 - 1, n3 - 1, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x24(s12, s23, s34, s14, s24, s13) * I4msls(n1, n2 - 1, n3, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13))

    if (n3 > 1)
      return ring(1) / ge3(s12, s23, s34, s14, s24, s13) / (n3 - 1) *
        (DerDet4x3(s12, s23, s34, s14, s24, s13) * I4msls(n1, n2, n3 - 1, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x31(s12, s23, s34, s14, s24, s13) * I4msls(n1 - 1, n2, n3 - 1, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x32(s12, s23, s34, s14, s24, s13) * I4msls(n1, n2 - 1, n3 - 1, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x33(s12, s23, s34, s14, s24, s13) * I4msls(n1, n2, n3 - 2, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x34(s12, s23, s34, s14, s24, s13) * I4msls(n1, n2, n3 - 1, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13))

    if (n4 > 1)
      return ring(1) / ge3(s12, s23, s34, s14, s24, s13) / (n4 - 1) *
        (DerDet4x4(s12, s23, s34, s14, s24, s13) * I4msls(n1, n2, n3, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x41(s12, s23, s34, s14, s24, s13) * I4msls(n1 - 1, n2, n3, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x42(s12, s23, s34, s14, s24, s13) * I4msls(n1, n2 - 1, n3, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x43(s12, s23, s34, s14, s24, s13) * I4msls(n1, n2, n3 - 1, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x44(s12, s23, s34, s14, s24, s13) * I4msls(n1, n2, n3, n4 - 2, d - 2, s12, s23, s34, s14, s24, s13))


    if ((n1, n2, n3, n4) == (1, 1, 1, 1)) {
      val b = ring(d.numerator().cc())
      if (b.signum() < 0)
        return ring(1) / 2 / DerDet4x0(s12, s23, s34, s14, s24, s13) *
          (ge3(s12, s23, s34, s14, s24, s13) * (d - 3) * I4msls(1, 1, 1, 1, d + 2, s12, s23, s34, s14, s24, s13)
            - DerDet4x1(s12, s23, s34, s14, s24, s13) * II3msls(1, 1, 1, d, s34, s24, s23)
            - DerDet4x2(s12, s23, s34, s14, s24, s13) * II3msls(1, 1, 1, d, s34, s14, s13)
            - DerDet4x3(s12, s23, s34, s14, s24, s13) * II3msls(1, 1, 1, d, s24, s14, s12)
            - DerDet4x4(s12, s23, s34, s14, s24, s13) * II3msls(1, 1, 1, d, s23, s13, s12))

      if (b.signum() > 0)
        return ring(1) / (d - 5) / ge3(s12, s23, s34, s14, s24, s13) *
          (DerDet4x0(s12, s23, s34, s14, s24, s13) * I4msls(1, 1, 1, 1, d - 2, s12, s23, s34, s14, s24, s13) * 2
            + DerDet4x1(s12, s23, s34, s14, s24, s13) * II3msls(1, 1, 1, d - 2, s34, s24, s23)
            + DerDet4x2(s12, s23, s34, s14, s24, s13) * II3msls(1, 1, 1, d - 2, s34, s14, s13)
            + DerDet4x3(s12, s23, s34, s14, s24, s13) * II3msls(1, 1, 1, d - 2, s24, s14, s12)
            + DerDet4x4(s12, s23, s34, s14, s24, s13) * II3msls(1, 1, 1, d - 2, s23, s13, s12))
    }

    return "I4msls".func(ring(n1), ring(n2), ring(n3), ring(n4), d, s12, s23, s34, s14, s24, s13)
  }


  /** compatibility with Tarasov's namings */
  private[fivepoint]
  def II4msls(n1: Int, n2: Int, n3: Int, n4: Int, d: Rat,
              s12: Rat, s23: Rat, s34: Rat, s14: Rat, s24: Rat, s13: Rat): Ret = I4msls(n1, n2, n3, n4, d, s12, s23, s34, s14, s24, s13)


  /** Computes 3-point massless integral, first looking into a cache */
  def I3msls(n1: Int, n2: Int, n3: Int, d: Rat, s23: Rat, s13: Rat, s12: Rat): Ret = {

    val cacheKey = (n1, n2, n3, d)
    cachedI3.get(cacheKey) match {
      case Some(cached) => cached(Seq(d, s23, s13, s12))
      case None =>
        val integral = __I3msls(n1, n2, n3, d, s23, s13, s12)
        cachedI3 += (cacheKey -> CachedExpression(
          "I3msls".func(ring(n1), ring(n2), ring(n3), d, s23, s13, s12),
          integral,
          Seq(d, s23, s13, s12)))
        integral
    }
  }

  /** actual calculation of 3-point massless integral */
  private[fivepoint]
  def __I3msls(n1: Int, n2: Int, n3: Int, d: Rat, s23: Rat, s13: Rat, s12: Rat): Ret = {
    assertInput(s23, s13, s12)

    if (n1 == 0)
      return I2msls(n2, n3, d, s23)

    if (n2 == 0)
      return I2msls(n1, n3, d, s13)

    if (n3 == 0)
      return I2msls(n1, n2, d, s12)

    if (ring.isZero(s12) && !(ring.isZero(s23) && ring.isZero(s13)))
      return ring(-2) * (d - 3) / (d - 4) / (s23 - s13) * (I2msls(1, 1, d, s23) - I2msls(1, 1, d, s13))

    if (ring.isZero(s13) && !(ring.isZero(s23) && ring.isZero(s12)))
      return ring(-2) * (d - 3) / (d - 4) / (s23 - s12) * (I2msls(1, 1, d, s23) - I2msls(1, 1, d, s12))

    if (ring.isZero(s23) && !(ring.isZero(s13) && ring.isZero(s12)))
      return ring(-2) * (d - 3) / (d - 4) / (s13 - s12) * (I2msls(1, 1, d, s13) - I2msls(1, 1, d, s12))


    if (n1 > 1)
      return ring(1) / ge2(s23, s13, s12) / (n1 - 1) *
        (DerDet3x1(s23, s13, s12) * I3msls(n1 - 1, n2, n3, d - 2, s23, s13, s12)
          + DerDet3x11(s23, s13, s12) * I3msls(n1 - 2, n2, n3, d - 2, s23, s13, s12)
          + DerDet3x12(s23, s13, s12) * I3msls(n1 - 1, n2 - 1, n3, d - 2, s23, s13, s12)
          + DerDet3x13(s23, s13, s12) * I3msls(n1 - 1, n2, n3 - 1, d - 2, s23, s13, s12))

    if (n2 > 1)
      return ring(1) / ge2(s23, s13, s12) / (n2 - 1) * (DerDet3x2(s23, s13, s12)
        * I3msls(n1, n2 - 1, n3, d - 2, s23, s13, s12)
        + DerDet3x21(s23, s13, s12) * I3msls(n1 - 1, n2 - 1, n3, d - 2, s23, s13, s12)
        + DerDet3x22(s23, s13, s12) * I3msls(n1, n2 - 2, n3, d - 2, s23, s13, s12)
        + DerDet3x23(s23, s13, s12) * I3msls(n1, n2 - 1, n3 - 1, d - 2, s23, s13, s12))


    if (n3 > 1)
      return ring(1) / ge2(s23, s13, s12) / (n3 - 1) *
        (DerDet3x3(s23, s13, s12) * I3msls(n1, n2, n3 - 1, d - 2, s23, s13, s12)
          + DerDet3x31(s23, s13, s12) * I3msls(n1 - 1, n2, n3 - 1, d - 2, s23, s13, s12)
          + DerDet3x32(s23, s13, s12) * I3msls(n1, n2 - 1, n3 - 1, d - 2, s23, s13, s12)
          + DerDet3x33(s23, s13, s12) * I3msls(n1, n2, n3 - 2, d - 2, s23, s13, s12))

    if ((n1, n2, n3) == (1, 1, 1)) {
      val b = ring(d.numerator().cc())
      if (b.numerator().lc().signum() < 0)
        return ring(1) / 2 / DerDet3x0(s23, s13, s12) *
          ((d - 2) * ge2(s23, s13, s12) * I3msls(1, 1, 1, d + 2, s23, s13, s12)
            - DerDet3x1(s23, s13, s12) * I2msls(1, 1, d, s23)
            - DerDet3x2(s23, s13, s12) * I2msls(1, 1, d, s13)
            - DerDet3x3(s23, s13, s12) * I2msls(1, 1, d, s12))

      if (b.numerator().lc().signum() > 0)
        return ring(1) / (d - 4) / ge2(s23, s13, s12) * (
          DerDet3x0(s23, s13, s12) * I3msls(1, 1, 1, d - 2, s23, s13, s12) * 2
            + DerDet3x1(s23, s13, s12) * I2msls(1, 1, d - 2, s23)
            + DerDet3x2(s23, s13, s12) * I2msls(1, 1, d - 2, s13)
            + DerDet3x3(s23, s13, s12) * I2msls(1, 1, d - 2, s12))
    }

    return "I3msls".func(ring(n1), ring(n2), ring(n3), d, s23, s13, s12)
  }

  /** compatibility with Tarasov's namings */
  private[fivepoint]
  def II3msls(n1: Int, n2: Int, n3: Int, d: Rat, s23: Rat, s13: Rat, s12: Rat) = I3msls(n1, n2, n3, d, s23, s13, s12)

  def aI2msls(n1: Int, n2: Int, de: Rat, qq: Rat): Ret = {
    assertInput(qq)

    if (qq.isZero || n1 <= 0 || n2 <= 0)
      return SymFuncSum.zero
    val d = ring("d")
    val b = (de - d).toString.toInt
    ring(-1).pow(b / 2) * qq.pow(b / 2 + 2 - n1 - n2) *
      Pochhammer(d / 2 - 1, 1 + b / 2 - n1) *
      Pochhammer(d / 2 - 1, 1 + b / 2 - n2) *
      Pochhammer(2 - d / 2, n1 + n2 - b / 2 - 2) /
      Pochhammer(d - 2, 2 + b - n1 - n2) /
      ring.factorial(n1 - 1) /
      ring.factorial(n2 - 1) *
      "i2msls".func(ring(1), ring(1), d, qq)
  }

  def I2msls(n1: Int, n2: Int, d: Rat, s: Rat): Ret = aI2msls(n1, n2, d, s)

  def Pochhammer(q: Rat, n: Int): Rat = n match {
    case 0 => ring(1)
    case _ if n > 0 => ring.multiply((0 until n).map(q + _): _*)
    case _ if n < 0 => ring(1) / Pochhammer(q + n, -n)
  }
}
