package cc.redberry.rings.sym

import java.io.Closeable

import cc.redberry.rings.legs.{CachedFactorizedIntegralVal, CachedIntegralVal, IntegralSignature, IntegralsDB}
import cc.redberry.rings.scaladsl.syntax._
import cc.redberry.rings.scaladsl.{Ring, _}
import cc.redberry.rings.sym.Definitions._

import scala.language.implicitConversions


/** Methods for reducing massless integrals */
class MasslessIntegrals[E](cfRing: Ring[E],
                           databaseFile: Option[String] = None,
                           twoPointDef: String = "I2",
                           threePointDef: String = "I3",
                           fourPointDef: String = "I4",
                           fivePointDef: String = "I5")
  extends Closeable {

  // import determinants
  lazy val det3_methods = new Determinants3x3(cfRing)
  lazy val det4_methods = new Determinants4x4(cfRing)
  lazy val det5_methods = new Determinants5x5(cfRing)

  import det3_methods._
  import det4_methods._
  import det5_methods._

  /** The underlying auxiliary ring: polynomials in 10 kinematic invariants + space-time dimension */
  val polyRing = MultivariateRing(cfRing, Array("d", "s12", "s23", "s34", "s45", "s15", "s13", "s14", "s24", "s25", "s35"))
  /** Field of rational functions */
  implicit val ring: Frac[MultivariatePolynomial[E]] = Frac(polyRing)
  /** Polynomial type */
  type Poly = polyRing.ElementType
  /** Type of mathematica expressions involved (rational functions) */
  type Expr = ring.ElementType

  object momentums {
    val (s12, s23, s34, s45, s15) = ring("s12", "s23", "s34", "s45", "s15")
    val (s13, s14, s24, s25, s35) = ring("s13", "s14", "s24", "s25", "s35")
    val dim = ring("d")
  }

  /** some simple checks */
  private
  def assertInput(seq: Expr*): Unit = assert(seq.forall(r => r.isIntegral && r.numerator().isVariable))

  /** logging */
  private
  def log(msg: String): Unit = System.err.println(msg)

  /**
    * Global off-heap storage of calculated integrals
    */
  private
  final lazy val database = databaseFile match {
    case Some(file) => Some(new IntegralsDB(file))
    case None => None
  }

  /** extract signature for caching */
  private
  def integralSignature(iDef: IntegralDef[Expr]) = IntegralSignature[Expr](iDef.id, iDef.indices, iDef.args.slice(0, 1))

  /** retrieves (factorized) integral value from db or computes it from lambda */
  private
  def getOrCompute(integral: IntegralDef[Expr],
                   computeRaw: () => IntegralVal[Expr],
                   needFactorized: Boolean)
  : Either[IntegralVal[Expr], FactorizedIntegralVal[Expr]]
  = database match {
    case Some(db) if integral.indices.product != 0 =>
      val signature: IntegralSignature[Expr] = integralSignature(integral)

      if (needFactorized) {
        // get or compute simplified expression (with factorized coefficients)

        val cachedIntegral: CachedFactorizedIntegralVal[Expr] = db.getSimplifiedIntegral(signature, ring)
        if (cachedIntegral != null) {
          log("Using cached value for factorized " + integral.stringify)
          Right(cachedIntegral.iFactorVals.map(r => Util.replaceVariables(r, cachedIntegral.iDef.args, integral.args)))
        } else {
          val raw = getOrCompute(integral, computeRaw, needFactorized = false).left.get
          log("Factorizing " + integral.stringify)
          val result = FactorizedIntegralVal.Factor(raw)
          db.putSimplifiedIntegral(signature, CachedFactorizedIntegralVal(integral, result), ring)
          Right(result)
        }
      } else {
        // get or compute raw expression (with not factorized coefficients)

        val cachedIntegral: CachedIntegralVal[Expr] = db.getRawIntegral(signature, ring)
        if (cachedIntegral != null) {
          log("Using cached value for " + integral.stringify)
          Left(cachedIntegral.iVal.map(r => Util.replaceVariables(r, cachedIntegral.iDef.args, integral.args)))
        } else {
          log("Computing " + integral.stringify)
          val result = computeRaw()
          db.putRawIntegral(signature, CachedIntegralVal(integral, result), ring)
          Left(result)
        }
      }

    case _ =>
      log("Computing " + integral.stringify)
      val raw = computeRaw()
      needFactorized match {
        case false =>
          Left(raw)
        case true =>
          log("Factorizing " + integral.stringify)
          Right(FactorizedIntegralVal.Factor(raw))
      }
  }

  override def close(): Unit = database match {
    case Some(db) => db.close() // this will commit all transactions
    case _ =>
  }

  /** Computes 5-point massless integral, first looking into a cache */
  def I5(n1: Int, n2: Int, n3: Int, n4: Int, n5: Int, d: Expr,
         s12: Expr, s23: Expr, s34: Expr, s45: Expr, s15: Expr,
         s13: Expr, s14: Expr, s24: Expr, s25: Expr, s35: Expr,
         factorized: Boolean = false)
  : Either[IntegralVal[Expr], FactorizedIntegralVal[Expr]] = {
    // integral definition
    val iDef = IntegralDef(fivePointDef, Seq(n1, n2, n3, n4, n5), Seq(d, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))
    // get from cache or compute from scratch
    getOrCompute(iDef, () => __I5(iDef, n1, n2, n3, n4, n5, d, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35), factorized)
  }

  /** Computes 5-point massless integral, first looking into a cache */
  private
  def _I5(n1: Int, n2: Int, n3: Int, n4: Int, n5: Int, d: Expr,
          s12: Expr, s23: Expr, s34: Expr, s45: Expr, s15: Expr,
          s13: Expr, s14: Expr, s24: Expr, s25: Expr, s35: Expr)
  : IntegralVal[Expr] =
    I5(n1, n2, n3, n4, n5, d, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35, factorized = false).left.get

  /** actual calculation of 5-point massless integral */
  private
  def __I5(iDef: IntegralDef[Expr],
           n1: Int, n2: Int, n3: Int, n4: Int, n5: Int, d: Expr,
           s12: Expr, s23: Expr, s34: Expr, s45: Expr, s15: Expr,
           s13: Expr, s14: Expr, s24: Expr, s25: Expr, s35: Expr): IntegralVal[Expr] = {

    assertInput(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)

    if (n1 == 0)
      return _I4(n2, n3, n4, n5, d, s23, s34, s45, s25, s35, s24)

    if (n2 == 0)
      return _I4(n1, n3, n4, n5, d, s13, s34, s45, s15, s35, s14)

    if (n3 == 0)
      return _I4(n1, n2, n4, n5, d, s12, s24, s45, s15, s25, s14)

    if (n4 == 0)
      return _I4(n1, n2, n3, n5, d, s12, s23, s35, s15, s25, s13)

    if (n5 == 0)
      return _I4(n1, n2, n3, n4, d, s12, s23, s34, s14, s24, s13)

    if (n1 > 1)
      return ring(1) /
        ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n1 - 1) *
        (DerDet5x1(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x11(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 2, n2, n3, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x12(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2 - 1, n3, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x13(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3 - 1, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x14(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x15(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if (n2 > 1)
      return ring(1) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n2 - 1) *
        (DerDet5x2(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x21(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2 - 1, n3, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x22(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 2, n3, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x23(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3 - 1, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x24(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x25(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if (n3 > 1)
      return ring(1) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n3 - 1) *
        (DerDet5x3(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3 - 1, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x31(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3 - 1, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x32(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3 - 1, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x33(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3 - 2, n4, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x34(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3 - 1, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x35(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3 - 1, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if (n4 > 1)
      return ring(1) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n4 - 1) *
        (DerDet5x4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x41(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x42(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x43(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3 - 1, n4 - 1, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x44(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3, n4 - 2, n5, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x45(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3, n4 - 1, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if (n5 > 1)
      return ring(1) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n5 - 1) *
        (DerDet5x5(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x51(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x52(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x53(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3 - 1, n4, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x54(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3, n4 - 1, n5 - 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x55(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3, n4, n5 - 2, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if ((n1, n2, n3, n4, n5) == (1, 1, 1, 1, 1)) {
      val b = d.numerator().cc()
      if (cfRing.signum(b) < 0)
        return ring(1) / 2 / DerDet5x0(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) *
          (ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * (d - 4) * _I5(1, 1, 1, 1, 1, d + 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
            - DerDet5x1(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, d, s23, s34, s45, s25, s35, s24)
            - DerDet5x2(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, d, s13, s34, s45, s15, s35, s14)
            - DerDet5x3(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, d, s12, s24, s45, s15, s25, s14)
            - DerDet5x4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, d, s12, s23, s35, s15, s25, s13)
            - DerDet5x5(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, d, s12, s23, s34, s14, s24, s13))

      if (cfRing.signum(b) > 0)
        return ring(1) / (d - 6) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) *
          (DerDet5x0(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(1, 1, 1, 1, 1, d - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * 2
            + DerDet5x1(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, d - 2, s23, s34, s45, s25, s35, s24)
            + DerDet5x2(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, d - 2, s13, s34, s45, s15, s35, s14)
            + DerDet5x3(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, d - 2, s12, s24, s45, s15, s25, s14)
            + DerDet5x4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, d - 2, s12, s23, s35, s15, s25, s13)
            + DerDet5x5(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, d - 2, s12, s23, s34, s14, s24, s13))
    }

    IntegralVal(iDef)
  }

  /** Computes 4-point massless integral, first looking into a cache */
  def I4(n1: Int, n2: Int, n3: Int, n4: Int, d: Expr,
         s12: Expr, s23: Expr, s34: Expr, s14: Expr, s24: Expr, s13: Expr,
         factorized: Boolean = false)
  : Either[IntegralVal[Expr], FactorizedIntegralVal[Expr]] = {
    // integral definition
    val iDef = IntegralDef(fourPointDef, Seq(n1, n2, n3, n4), Seq(d, s12, s23, s34, s14, s24, s13))
    // get from cache or compute from scratch
    getOrCompute(iDef, () => __I4(iDef, n1, n2, n3, n4, d, s12, s23, s34, s14, s24, s13), factorized)
  }

  /** Computes 4-point massless integral, first looking into a cache */
  private
  def _I4(n1: Int, n2: Int, n3: Int, n4: Int, d: Expr,
          s12: Expr, s23: Expr, s34: Expr, s14: Expr, s24: Expr, s13: Expr)
  : IntegralVal[Expr] =
    I4(n1, n2, n3, n4, d, s12, s23, s34, s14, s24, s13, factorized = false).left.get

  /** actual calculation of 4-point massless integral */
  private
  def __I4(iDef: IntegralDef[Expr],
           n1: Int, n2: Int, n3: Int, n4: Int, d: Expr,
           s12: Expr, s23: Expr, s34: Expr, s14: Expr, s24: Expr, s13: Expr): IntegralVal[Expr] = {

    assertInput(s12, s23, s34, s14, s24, s34, s14, s24, s13)

    if (n1 == 0)
      return _I3(n2, n3, n4, d, s34, s24, s23)

    if (n2 == 0)
      return _I3(n1, n3, n4, d, s34, s14, s13)

    if (n3 == 0)
      return _I3(n1, n2, n4, d, s24, s14, s12)

    if (n4 == 0)
      return _I3(n1, n2, n3, d, s23, s13, s12)


    if (n1 > 1)
      return ring(1) / ge3(s12, s23, s34, s14, s24, s13) / (n1 - 1) *
        (DerDet4x1(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2, n3, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x11(s12, s23, s34, s14, s24, s13) * _I4(n1 - 2, n2, n3, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x12(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2 - 1, n3, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x13(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2, n3 - 1, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x14(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2, n3, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13))

    if (n2 > 1)
      return ring(1) / ge3(s12, s23, s34, s14, s24, s13) / (n2 - 1) *
        (DerDet4x2(s12, s23, s34, s14, s24, s13) * _I4(n1, n2 - 1, n3, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x21(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2 - 1, n3, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x22(s12, s23, s34, s14, s24, s13) * _I4(n1, n2 - 2, n3, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x23(s12, s23, s34, s14, s24, s13) * _I4(n1, n2 - 1, n3 - 1, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x24(s12, s23, s34, s14, s24, s13) * _I4(n1, n2 - 1, n3, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13))

    if (n3 > 1)
      return ring(1) / ge3(s12, s23, s34, s14, s24, s13) / (n3 - 1) *
        (DerDet4x3(s12, s23, s34, s14, s24, s13) * _I4(n1, n2, n3 - 1, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x31(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2, n3 - 1, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x32(s12, s23, s34, s14, s24, s13) * _I4(n1, n2 - 1, n3 - 1, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x33(s12, s23, s34, s14, s24, s13) * _I4(n1, n2, n3 - 2, n4, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x34(s12, s23, s34, s14, s24, s13) * _I4(n1, n2, n3 - 1, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13))

    if (n4 > 1)
      return ring(1) / ge3(s12, s23, s34, s14, s24, s13) / (n4 - 1) *
        (DerDet4x4(s12, s23, s34, s14, s24, s13) * _I4(n1, n2, n3, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x41(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2, n3, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x42(s12, s23, s34, s14, s24, s13) * _I4(n1, n2 - 1, n3, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x43(s12, s23, s34, s14, s24, s13) * _I4(n1, n2, n3 - 1, n4 - 1, d - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x44(s12, s23, s34, s14, s24, s13) * _I4(n1, n2, n3, n4 - 2, d - 2, s12, s23, s34, s14, s24, s13))


    if ((n1, n2, n3, n4) == (1, 1, 1, 1)) {
      val b = d.numerator().cc()
      if (cfRing.signum(b) < 0)
        return ring(1) / 2 / DerDet4x0(s12, s23, s34, s14, s24, s13) *
          (ge3(s12, s23, s34, s14, s24, s13) * (d - 3) * _I4(1, 1, 1, 1, d + 2, s12, s23, s34, s14, s24, s13)
            - DerDet4x1(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, d, s34, s24, s23)
            - DerDet4x2(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, d, s34, s14, s13)
            - DerDet4x3(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, d, s24, s14, s12)
            - DerDet4x4(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, d, s23, s13, s12))

      if (cfRing.signum(b) > 0)
        return ring(1) / (d - 5) / ge3(s12, s23, s34, s14, s24, s13) *
          (DerDet4x0(s12, s23, s34, s14, s24, s13) * _I4(1, 1, 1, 1, d - 2, s12, s23, s34, s14, s24, s13) * 2
            + DerDet4x1(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, d - 2, s34, s24, s23)
            + DerDet4x2(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, d - 2, s34, s14, s13)
            + DerDet4x3(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, d - 2, s24, s14, s12)
            + DerDet4x4(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, d - 2, s23, s13, s12))
    }

    IntegralVal(iDef)
  }

  /** Computes 3-point massless integral, first looking into a cache */
  def I3(n1: Int, n2: Int, n3: Int, d: Expr, s23: Expr, s13: Expr, s12: Expr, factorized: Boolean = false)
  : Either[IntegralVal[Expr], FactorizedIntegralVal[Expr]] = {
    // integral definition
    val iDef = IntegralDef(threePointDef, Seq(n1, n2, n3), Seq(d, s23, s13, s12))
    // get from cache or compute from scratch
    getOrCompute(iDef, () => __I3(iDef, n1, n2, n3, d, s23, s13, s12), factorized)
  }

  /** Computes 3-point massless integral, first looking into a cache */
  private
  def _I3(n1: Int, n2: Int, n3: Int, d: Expr, s23: Expr, s13: Expr, s12: Expr)
  : IntegralVal[Expr] =
    I3(n1, n2, n3, d, s23, s13, s12, factorized = false).left.get

  /** actual calculation of 3-point massless integral */
  private
  def __I3(iDef: IntegralDef[Expr], n1: Int, n2: Int, n3: Int, d: Expr, s23: Expr, s13: Expr, s12: Expr): IntegralVal[Expr] = {
    assertInput(s23, s13, s12)

    if (n1 == 0)
      return I2(n2, n3, d, s23)

    if (n2 == 0)
      return I2(n1, n3, d, s13)

    if (n3 == 0)
      return I2(n1, n2, d, s12)

    if (ring.isZero(s12) && !(ring.isZero(s23) && ring.isZero(s13)))
      return ring(-2) * (d - 3) / (d - 4) / (s23 - s13) * (I2(1, 1, d, s23) - I2(1, 1, d, s13))

    if (ring.isZero(s13) && !(ring.isZero(s23) && ring.isZero(s12)))
      return ring(-2) * (d - 3) / (d - 4) / (s23 - s12) * (I2(1, 1, d, s23) - I2(1, 1, d, s12))

    if (ring.isZero(s23) && !(ring.isZero(s13) && ring.isZero(s12)))
      return ring(-2) * (d - 3) / (d - 4) / (s13 - s12) * (I2(1, 1, d, s13) - I2(1, 1, d, s12))


    if (n1 > 1)
      return ring(1) / ge2(s23, s13, s12) / (n1 - 1) *
        (DerDet3x1(s23, s13, s12) * _I3(n1 - 1, n2, n3, d - 2, s23, s13, s12)
          + DerDet3x11(s23, s13, s12) * _I3(n1 - 2, n2, n3, d - 2, s23, s13, s12)
          + DerDet3x12(s23, s13, s12) * _I3(n1 - 1, n2 - 1, n3, d - 2, s23, s13, s12)
          + DerDet3x13(s23, s13, s12) * _I3(n1 - 1, n2, n3 - 1, d - 2, s23, s13, s12))

    if (n2 > 1)
      return ring(1) / ge2(s23, s13, s12) / (n2 - 1) * (DerDet3x2(s23, s13, s12)
        * _I3(n1, n2 - 1, n3, d - 2, s23, s13, s12)
        + DerDet3x21(s23, s13, s12) * _I3(n1 - 1, n2 - 1, n3, d - 2, s23, s13, s12)
        + DerDet3x22(s23, s13, s12) * _I3(n1, n2 - 2, n3, d - 2, s23, s13, s12)
        + DerDet3x23(s23, s13, s12) * _I3(n1, n2 - 1, n3 - 1, d - 2, s23, s13, s12))


    if (n3 > 1)
      return ring(1) / ge2(s23, s13, s12) / (n3 - 1) *
        (DerDet3x3(s23, s13, s12) * _I3(n1, n2, n3 - 1, d - 2, s23, s13, s12)
          + DerDet3x31(s23, s13, s12) * _I3(n1 - 1, n2, n3 - 1, d - 2, s23, s13, s12)
          + DerDet3x32(s23, s13, s12) * _I3(n1, n2 - 1, n3 - 1, d - 2, s23, s13, s12)
          + DerDet3x33(s23, s13, s12) * _I3(n1, n2, n3 - 2, d - 2, s23, s13, s12))

    if ((n1, n2, n3) == (1, 1, 1)) {
      val b = d.numerator().cc()
      if (cfRing.signum(b) < 0)
        return ring(1) / 2 / DerDet3x0(s23, s13, s12) *
          ((d - 2) * ge2(s23, s13, s12) * _I3(1, 1, 1, d + 2, s23, s13, s12)
            - DerDet3x1(s23, s13, s12) * I2(1, 1, d, s23)
            - DerDet3x2(s23, s13, s12) * I2(1, 1, d, s13)
            - DerDet3x3(s23, s13, s12) * I2(1, 1, d, s12))

      if (cfRing.signum(b) > 0)
        return ring(1) / (d - 4) / ge2(s23, s13, s12) * (
          DerDet3x0(s23, s13, s12) * _I3(1, 1, 1, d - 2, s23, s13, s12) * 2
            + DerDet3x1(s23, s13, s12) * I2(1, 1, d - 2, s23)
            + DerDet3x2(s23, s13, s12) * I2(1, 1, d - 2, s13)
            + DerDet3x3(s23, s13, s12) * I2(1, 1, d - 2, s12))
    }

    IntegralVal(iDef)
  }

  /** Two-point integral */
  def I2(n1: Int, n2: Int, de: Expr, qq: Expr): IntegralVal[Expr] = {
    assertInput(qq)

    if (qq.isZero || n1 <= 0 || n2 <= 0)
      return IntegralVal.zero
    val d = momentums.dim
    val b = (de - d).toString.toInt
    ring(-1).pow(b / 2) * qq.pow(b / 2 + 2 - n1 - n2) *
      Pochhammer(d / 2 - 1, 1 + b / 2 - n1) *
      Pochhammer(d / 2 - 1, 1 + b / 2 - n2) *
      Pochhammer(2 - d / 2, n1 + n2 - b / 2 - 2) /
      Pochhammer(d - 2, 2 + b - n1 - n2) /
      ring.factorial(n1 - 1) /
      ring.factorial(n2 - 1) *
      IntegralVal(twoPointDef, Seq(1, 1), Seq(d, qq))
  }

  private
  def Pochhammer(q: Expr, n: Int): Expr = n match {
    case 0 => ring(1)
    case _ if n > 0 => ring.multiply((0 until n).map(q + _): _*)
    case _ if n < 0 => ring(1) / Pochhammer(q + n, -n)
  }
}

object Util {
  /** auxiliary method used to shuffle variables (from <-> to) */
  def replaceVariables[E](expr: MultivariatePolynomial[E],
                          from: Seq[MultivariatePolynomial[E]],
                          to: Seq[MultivariatePolynomial[E]])
  : MultivariatePolynomial[E] = {

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
  def replaceVariables[E](expr: Rational[MultivariatePolynomial[E]],
                          from: Seq[Rational[MultivariatePolynomial[E]]],
                          to: Seq[Rational[MultivariatePolynomial[E]]])
  : Rational[MultivariatePolynomial[E]] = {
    val _from = from.map(_.numerator())
    val _to = to.map(_.numerator())
    expr.map((f: MultivariatePolynomial[E]) => replaceVariables(f, _from, _to))
  }
}