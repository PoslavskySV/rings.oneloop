package cc.redberry.rings.oneloop

import java.io.{Closeable, PrintStream}

import cc.redberry.core.context.OutputFormat
import cc.redberry.core.indices.IndicesUtils
import cc.redberry.core.parser.ParserIndices
import cc.redberry.core.tensor.Tensors._
import cc.redberry.core.tensor.{Tensor, Tensors}
import cc.redberry.core.transformations.DifferentiateTransformation.differentiate
import cc.redberry.core.utils.TensorUtils
import cc.redberry.rings.oneloop.Definitions.FactorizedIntegralVal.Factor
import cc.redberry.rings.oneloop.Definitions._
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.language.implicitConversions

/** Methods for reducing massless integrals */
class MasslessIntegrals[E](val cfRing: Ring[E],
                           val databaseFile: Option[String] = None,
                           val twoPointDef: String = "I2",
                           val threePointDef: String = "I3",
                           val fourPointDef: String = "I4",
                           val fivePointDef: String = "I5",
                           val usedVariables: Seq[String] = Seq("s12", "s23", "s34", "s45", "s15", "s13", "s14", "s24", "s25", "s35"),
                           val doLog: Boolean = true)
  extends Closeable {

  // import determinants
  lazy val det3_methods = new Determinants3x3(cfRing)
  lazy val det4_methods = new Determinants4x4(cfRing)
  lazy val det5_methods = new Determinants5x5(cfRing)

  import det3_methods._
  import det4_methods._
  import det5_methods._

  /** The underlying auxiliary ring: polynomials in 10 kinematic invariants + space-time dimension */
  val polyRing = MultivariateRing(cfRing, (Seq("d") ++ usedVariables).distinct.toArray)
  /** Field of rational functions */
  implicit val ring: Frac[MultivariatePolynomial[E]] = Frac(polyRing)
  log(s"Ring: $ring")
  /** Polynomial type */
  type Poly = polyRing.ElementType
  /** Type of mathematica expressions involved (rational functions) */
  type Expr = ring.ElementType
  /** "d" variable */
  val dimension = ring("d")
  /** Type of the result: either raw integral or factorized integral */
  type Result = Either[IntegralVal[Expr], FactorizedIntegralVal[Expr]]

  trait PrintableOps {
    def print(stream: PrintStream, formatter: PrintFormatter)
  }

  implicit class ResultOps(f1: Result) extends PrintableOps {
    def +(f2: Result) = f1 match {
      case Right(_) => Factorized(f1.right.get + f2.right.get)
      case Left(_) => NonFactorized(f1.left.get + f2.left.get)
    }

    def *(f2: Expr) = f1 match {
      case Right(_) => Factorized(f1.right.get * f2)
      case Left(_) => NonFactorized(f1.left.get * f2)
    }

    def zero() = f1 match {
      case Right(_) => Factorized(FactorizedIntegralVal.zero[Expr])
      case Left(_) => NonFactorized(IntegralVal.zero[Expr])
    }

    def print(stream: PrintStream, formatter: PrintFormatter) = f1 match {
      case Left(v) => v.print(stream, formatter)
      case Right(v) => v.print(stream, formatter)
    }
  }

  def Factorized(e: FactorizedIntegralVal[Expr]): Result = Right(e)

  def NonFactorized(e: IntegralVal[Expr]): Result = Left(e)

  /** some simple checks */
  private
  def assertInput(seq: Expr*): Unit = {} //assert(seq.forall(r => r.isIntegral && r.numerator().isVariable))

  /** logging */
  private
  def log(msg: String): Unit = if (doLog) Console.err.println(msg)

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
  def mkSignature(iDef: IntegralDef[Expr]) =
    IntegralSignature[Expr](iDef.id, iDef.indices, iDef.args.slice(0, 1), iDef.args.zipWithIndex.filter(_._1.isZero).map(_._2))

  private
  def isGeneric(iDef: IntegralDef[Expr]): Boolean = {
    // non-zero kinematic invariants
    val args = iDef.args.drop(1).filterNot(_.isZero)

    if (args.exists(p => !p.isIntegral || p.numerator().isConstant || !p.numerator().isVariable))
      return false

    if (args.distinct.length != args.length)
      return false

    true
  }

  /** retrieves (factorized) integral value from db or computes it from lambda */
  private
  def getOrCompute(integral: IntegralDef[Expr],
                   computeRaw: () => IntegralVal[Expr],
                   needFactorized: Boolean)
  : Result
  = database match {
    case Some(db) if integral.indices.product != 0 =>
      val isGeneric = this.isGeneric(integral)
      val signature =
        if (isGeneric)
          mkSignature(integral)
        else
          IntegralSignature(integral)

      if (needFactorized) {
        // get or compute simplified expression (with factorized coefficients)

        val cachedIntegral: CachedFactorizedIntegralVal[Expr] = db.getFactorization(isGeneric, signature, ring)
        if (cachedIntegral != null) {
          log("Using cached value for factorized " + integral.stringify)

          if (isGeneric) // map generic integral
            Factorized(cachedIntegral.iFactorVals.map(r => Util.replaceVariables(r, cachedIntegral.iDef.args, integral.args)))

          else // return exact as is
            Factorized(cachedIntegral.iFactorVals)

        } else {
          val raw = getOrCompute(integral, computeRaw, needFactorized = false).left.get
          log("Factorizing " + integral.stringify)
          val result = Factor(raw)
          db.putFactorization(isGeneric, signature, CachedFactorizedIntegralVal(integral, result), ring)
          Factorized(result)
        }
      } else {
        // get or compute raw expression (with not factorized coefficients)

        val cachedIntegral: CachedIntegralVal[Expr] = db.getIntegral(isGeneric, signature, ring)
        if (cachedIntegral != null) {
          log("Using cached value for " + integral.stringify)
          if (isGeneric) // map generic integral
            NonFactorized(cachedIntegral.iVal.map(r => Util.replaceVariables(r, cachedIntegral.iDef.args, integral.args)))
          else // return exact as is
            NonFactorized(cachedIntegral.iVal)
        } else {
          log("Computing " + integral.stringify)
          val result = wrappedCompute(integral, computeRaw)
          db.putIntegral(isGeneric, signature, CachedIntegralVal(integral, result), ring)
          NonFactorized(result)
        }
      }

    case _ =>
      log("Computing " + integral.stringify)
      val raw = wrappedCompute(integral, computeRaw)
      needFactorized match {
        case false =>
          NonFactorized(raw)
        case true =>
          log("Factorizing " + integral.stringify)
          Factorized(Factor(raw))
      }
  }

  override def close(): Unit = database match {
    case Some(db) => db.close() // this will commit all transactions
    case _ =>
  }

  /** substitute specified values `to` for `from` and tries to evaluate the result */
  def wrappedCompute(integral: IntegralDef[Expr],
                     computeRaw: () => IntegralVal[Expr]): IntegralVal[Expr] = {
    try {computeRaw()} catch {
      case ex: ArithmeticException =>
        log(s"WARN: some Gram determinants are zero; trying to compute the limit ${integral.stringify()}")
        val genericInvariants = defaultGenericArguments(integral)
        val genericIntegral = integral.copy(args = Seq(integral.args.head) ++ genericInvariants)
        val genericIntegralValue = evaluate(genericIntegral)._2.left.get

        if (genericIntegralValue.terms.size == 1) {
          log(s"WARN: some Gram determinants are zero for ${integral.stringify()}")
          throw ex
        }


        try {
          findLimit(genericIntegralValue, genericInvariants, integral.args.slice(1, integral.args.length))
        } catch {
          case ex: ArithmeticException =>
            log(s"WARN: some Gram determinants are zero for ${integral.stringify()}")
            throw ex
        }
    }
  }

  /** Evaluates given [[IntegralDef]] */
  private
  def defaultGenericArguments(iDef: IntegralDef[Expr]): Seq[Expr] = {
    val d = polyRing("d")
    polyRing.variables.map(polyRing(_)).filter(_ != d).slice(0, iDef.args.size - 1).map(ring.mkNumerator)
  }

  /** substitute specified values `to` for `from` and tries to evaluate the result */
  def findLimit(iVal: IntegralVal[Expr], _from: Seq[Expr], _to: Seq[Expr]): IntegralVal[Expr] = {
    if (_from.exists(p => !p.isIntegral || !p.numerator().isVariable))
      throw new IllegalArgumentException

    val notSame = (_from zip _to).zipWithIndex.filter(t => t._1._1 != t._1._2).map(_._2)
    val from = notSame collect _from
    val to = notSame collect _to

    val gr = to.zipWithIndex.groupBy(_._1.isIntegral).mapValues(_.map(_._2))
    val Seq(integral, rational) = Seq(true, false).map(gr.getOrElse(_, Seq.empty[Int]))

    def indices(es: Seq[Expr]) = es.map(_.numerator().univariateVariable()).toArray

    var result: IntegralVal[Expr] = iVal
    if (integral.nonEmpty)
      result = _evaluate(
        result,
        indices(integral collect from),
        (integral collect to).map(_.numerator()).toArray)
    if (rational.nonEmpty)
      result = _evaluate(
        result,
        indices(rational collect from),
        (rational collect to).toArray)

    result
  }

  private
  def _limit(expr: Expr, iVars: Array[Int], to: Array[Poly]) = {
    expr.map(_.composition(iVars, to))
  }

  private
  def _evaluate(iVal: IntegralVal[Expr], iVars: Array[Int], to: Array[Poly]): IntegralVal[Expr] = {
    iVal.
      terms.map { case (k, v) => evaluate(k.map(_.map(_.composition(iVars, to))))._2.left.get * _limit(v, iVars, to) }
      .foldLeft(IntegralVal.zero[Expr]) { _ + _ }
  }

  private
  def _evaluate(iVal: IntegralVal[Expr], iVars: Array[Int], to: Array[Expr]): IntegralVal[Expr] = {
    iVal.terms.map { case (k, v) => evaluate(k.map(e => _limitExpr(e, iVars, to)))._2.left.get * _limitExpr(v, iVars, to) }
      .foldLeft(IntegralVal.zero[Expr]) { _ + _ }
  }

  private
  def _limitExpr(e: Expr, iVars: Array[Int], to: Array[Expr]): Expr = {
    val vars: Array[Expr] = (0 until polyRing.nVariables()).map(i => ring.mkNumerator(polyRing.theRing.variable(i))).toArray
    for (i <- iVars.indices)
      vars(iVars(i)) = to(i)

    val Seq(num, den) = Seq(e.numerator(), e.denominator())
      .map(_.mapCoefficients[Expr](ring, c => ring.mkNumerator(polyRing.getConstant(c))).evaluate(vars: _*))

    num / den
  }

  /** Evaluates given [[IntegralDef]] */
  def evaluate(iDef: IntegralDef[Expr], factorized: Boolean = false): (IntegralDef[Expr], Result) = {
    iDef.id match {
      case id if id == fivePointDef => I5(
        iDef.indices(0), iDef.indices(1), iDef.indices(2), iDef.indices(3), iDef.indices(4),
        iDef.args(0).numerator().cc().toString.toInt,
        iDef.args(1), iDef.args(2), iDef.args(3), iDef.args(4), iDef.args(5),
        iDef.args(6), iDef.args(7), iDef.args(8), iDef.args(9), iDef.args(10),
        factorized)
      case id if id == fourPointDef => I4(
        iDef.indices(0), iDef.indices(1), iDef.indices(2), iDef.indices(3),
        iDef.args(0).numerator().cc().toString.toInt,
        iDef.args(1), iDef.args(2), iDef.args(3), iDef.args(4), iDef.args(5),
        iDef.args(6),
        factorized)
      case id if id == threePointDef => I3(
        iDef.indices(0), iDef.indices(1), iDef.indices(2),
        iDef.args(0).numerator().cc().toString.toInt,
        iDef.args(1), iDef.args(2), iDef.args(3),
        factorized)
      case id if id == twoPointDef => I2(
        iDef.indices(0), iDef.indices(1),
        iDef.args(0).numerator().cc().toString.toInt,
        iDef.args(1),
        factorized)
      case _ => ???
    }
  }

  private[oneloop]
  def mkI5def(n1: Int, n2: Int, n3: Int, n4: Int, n5: Int, di: Int,
              s12: Expr, s23: Expr, s34: Expr, s45: Expr, s15: Expr,
              s13: Expr, s14: Expr, s24: Expr, s25: Expr, s35: Expr) = {
    val d = dimension + di
    IntegralDef(fivePointDef, Seq(n1, n2, n3, n4, n5), Seq(d, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))
  }

  /** Computes 5-point massless integral, first looking into a cache */
  def I5(n1: Int, n2: Int, n3: Int, n4: Int, n5: Int, di: Int,
         s12: Expr, s23: Expr, s34: Expr, s45: Expr, s15: Expr,
         s13: Expr, s14: Expr, s24: Expr, s25: Expr, s35: Expr,
         factorized: Boolean = false)
  : (IntegralDef[Expr], Result) = {
    // integral definition
    val iDef = mkI5def(n1, n2, n3, n4, n5, di, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
    // get from cache or compute from scratch
    (iDef, getOrCompute(iDef, () => __I5(iDef, n1, n2, n3, n4, n5, di, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35), factorized))
  }

  /** Computes 5-point massless integral, first looking into a cache */
  private
  def _I5(n1: Int, n2: Int, n3: Int, n4: Int, n5: Int, di: Int,
          s12: Expr, s23: Expr, s34: Expr, s45: Expr, s15: Expr,
          s13: Expr, s14: Expr, s24: Expr, s25: Expr, s35: Expr)
  : IntegralVal[Expr] = I5(n1, n2, n3, n4, n5, di, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35, factorized = false)._2.left.get

  /** actual calculation of 5-point massless integral */
  private
  def __I5(iDef: IntegralDef[Expr],
           n1: Int, n2: Int, n3: Int, n4: Int, n5: Int, di: Int,
           s12: Expr, s23: Expr, s34: Expr, s45: Expr, s15: Expr,
           s13: Expr, s14: Expr, s24: Expr, s25: Expr, s35: Expr): IntegralVal[Expr] = {

    assertInput(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)

    if (n1 == 0)
      return _I4(n2, n3, n4, n5, di, s23, s34, s45, s25, s35, s24)

    if (n2 == 0)
      return _I4(n1, n3, n4, n5, di, s13, s34, s45, s15, s35, s14)

    if (n3 == 0)
      return _I4(n1, n2, n4, n5, di, s12, s24, s45, s15, s25, s14)

    if (n4 == 0)
      return _I4(n1, n2, n3, n5, di, s12, s23, s35, s15, s25, s13)

    if (n5 == 0)
      return _I4(n1, n2, n3, n4, di, s12, s23, s34, s14, s24, s13)

    if (n1 > 1)
      return ring(1) /
        ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n1 - 1) *
        (DerDet5x1(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3, n4, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x11(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 2, n2, n3, n4, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x12(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2 - 1, n3, n4, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x13(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3 - 1, n4, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x14(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3, n4 - 1, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x15(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3, n4, n5 - 1, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if (n2 > 1)
      return ring(1) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n2 - 1) *
        (DerDet5x2(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3, n4, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x21(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2 - 1, n3, n4, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x22(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 2, n3, n4, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x23(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3 - 1, n4, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x24(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3, n4 - 1, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x25(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3, n4, n5 - 1, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if (n3 > 1)
      return ring(1) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n3 - 1) *
        (DerDet5x3(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3 - 1, n4, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x31(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3 - 1, n4, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x32(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3 - 1, n4, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x33(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3 - 2, n4, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x34(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3 - 1, n4 - 1, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x35(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3 - 1, n4, n5 - 1, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if (n4 > 1)
      return ring(1) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n4 - 1) *
        (DerDet5x4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3, n4 - 1, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x41(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3, n4 - 1, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x42(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3, n4 - 1, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x43(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3 - 1, n4 - 1, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x44(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3, n4 - 2, n5, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x45(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3, n4 - 1, n5 - 1, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if (n5 > 1)
      return ring(1) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) / (n5 - 1) *
        (DerDet5x5(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3, n4, n5 - 1, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x51(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1 - 1, n2, n3, n4, n5 - 1, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x52(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2 - 1, n3, n4, n5 - 1, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x53(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3 - 1, n4, n5 - 1, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x54(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3, n4 - 1, n5 - 1, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
          + DerDet5x55(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(n1, n2, n3, n4, n5 - 2, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35))

    if ((n1, n2, n3, n4, n5) == (1, 1, 1, 1, 1)) {
      val d = dimension + di
      if (di < 0)
        return ring(1) / 2 / DerDet5x0(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) *
          (ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * (d - 4) * _I5(1, 1, 1, 1, 1, di + 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35)
            - DerDet5x1(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, di, s23, s34, s45, s25, s35, s24)
            - DerDet5x2(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, di, s13, s34, s45, s15, s35, s14)
            - DerDet5x3(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, di, s12, s24, s45, s15, s25, s14)
            - DerDet5x4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, di, s12, s23, s35, s15, s25, s13)
            - DerDet5x5(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, di, s12, s23, s34, s14, s24, s13))

      if (di > 0)
        return ring(1) / (d - 6) / ge4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) *
          (DerDet5x0(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I5(1, 1, 1, 1, 1, di - 2, s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * 2
            + DerDet5x1(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, di - 2, s23, s34, s45, s25, s35, s24)
            + DerDet5x2(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, di - 2, s13, s34, s45, s15, s35, s14)
            + DerDet5x3(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, di - 2, s12, s24, s45, s15, s25, s14)
            + DerDet5x4(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, di - 2, s12, s23, s35, s15, s25, s13)
            + DerDet5x5(s12, s23, s34, s45, s15, s13, s14, s24, s25, s35) * _I4(1, 1, 1, 1, di - 2, s12, s23, s34, s14, s24, s13))
    }

    IntegralVal(iDef)
  }

  private[oneloop]
  def mkI4def(n1: Int, n2: Int, n3: Int, n4: Int, di: Int,
              s12: Expr, s23: Expr, s34: Expr, s14: Expr, s24: Expr, s13: Expr) = {
    val d = dimension + di
    IntegralDef(fourPointDef, Seq(n1, n2, n3, n4), Seq(d, s12, s23, s34, s14, s24, s13))
  }

  /** Computes 4-point massless integral, first looking into a cache */
  def I4(n1: Int, n2: Int, n3: Int, n4: Int, di: Int,
         s12: Expr, s23: Expr, s34: Expr, s14: Expr, s24: Expr, s13: Expr,
         factorized: Boolean = false)
  : (IntegralDef[Expr], Result) = {
    // integral definition
    val iDef = mkI4def(n1, n2, n3, n4, di, s12, s23, s34, s14, s24, s13)
    // get from cache or compute from scratch
    (iDef, getOrCompute(iDef, () => __I4(iDef, n1, n2, n3, n4, di, s12, s23, s34, s14, s24, s13), factorized))
  }

  /** Computes 4-point massless integral, first looking into a cache */
  private
  def _I4(n1: Int, n2: Int, n3: Int, n4: Int, di: Int,
          s12: Expr, s23: Expr, s34: Expr, s14: Expr, s24: Expr, s13: Expr)
  : IntegralVal[Expr] = I4(n1, n2, n3, n4, di, s12, s23, s34, s14, s24, s13, factorized = false)._2.left.get

  /** actual calculation of 4-point massless integral */
  private
  def __I4(iDef: IntegralDef[Expr],
           n1: Int, n2: Int, n3: Int, n4: Int, di: Int,
           s12: Expr, s23: Expr, s34: Expr, s14: Expr, s24: Expr, s13: Expr): IntegralVal[Expr] = {

    assertInput(s12, s23, s34, s14, s24, s34, s14, s24, s13)

    if (n1 == 0)
      return _I3(n2, n3, n4, di, s34, s24, s23)

    if (n2 == 0)
      return _I3(n1, n3, n4, di, s34, s14, s13)

    if (n3 == 0)
      return _I3(n1, n2, n4, di, s24, s14, s12)

    if (n4 == 0)
      return _I3(n1, n2, n3, di, s23, s13, s12)

    if (n1 > 1)
      return ring(1) / ge3(s12, s23, s34, s14, s24, s13) / (n1 - 1) *
        (DerDet4x1(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2, n3, n4, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x11(s12, s23, s34, s14, s24, s13) * _I4(n1 - 2, n2, n3, n4, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x12(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2 - 1, n3, n4, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x13(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2, n3 - 1, n4, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x14(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2, n3, n4 - 1, di - 2, s12, s23, s34, s14, s24, s13))

    if (n2 > 1)
      return ring(1) / ge3(s12, s23, s34, s14, s24, s13) / (n2 - 1) *
        (DerDet4x2(s12, s23, s34, s14, s24, s13) * _I4(n1, n2 - 1, n3, n4, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x21(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2 - 1, n3, n4, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x22(s12, s23, s34, s14, s24, s13) * _I4(n1, n2 - 2, n3, n4, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x23(s12, s23, s34, s14, s24, s13) * _I4(n1, n2 - 1, n3 - 1, n4, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x24(s12, s23, s34, s14, s24, s13) * _I4(n1, n2 - 1, n3, n4 - 1, di - 2, s12, s23, s34, s14, s24, s13))

    if (n3 > 1)
      return ring(1) / ge3(s12, s23, s34, s14, s24, s13) / (n3 - 1) *
        (DerDet4x3(s12, s23, s34, s14, s24, s13) * _I4(n1, n2, n3 - 1, n4, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x31(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2, n3 - 1, n4, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x32(s12, s23, s34, s14, s24, s13) * _I4(n1, n2 - 1, n3 - 1, n4, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x33(s12, s23, s34, s14, s24, s13) * _I4(n1, n2, n3 - 2, n4, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x34(s12, s23, s34, s14, s24, s13) * _I4(n1, n2, n3 - 1, n4 - 1, di - 2, s12, s23, s34, s14, s24, s13))

    if (n4 > 1)
      return ring(1) / ge3(s12, s23, s34, s14, s24, s13) / (n4 - 1) *
        (DerDet4x4(s12, s23, s34, s14, s24, s13) * _I4(n1, n2, n3, n4 - 1, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x41(s12, s23, s34, s14, s24, s13) * _I4(n1 - 1, n2, n3, n4 - 1, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x42(s12, s23, s34, s14, s24, s13) * _I4(n1, n2 - 1, n3, n4 - 1, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x43(s12, s23, s34, s14, s24, s13) * _I4(n1, n2, n3 - 1, n4 - 1, di - 2, s12, s23, s34, s14, s24, s13)
          + DerDet4x44(s12, s23, s34, s14, s24, s13) * _I4(n1, n2, n3, n4 - 2, di - 2, s12, s23, s34, s14, s24, s13))


    if ((n1, n2, n3, n4) == (1, 1, 1, 1)) {
      val d = dimension + di
      if (di < 0)
        return ring(1) / 2 / DerDet4x0(s12, s23, s34, s14, s24, s13) *
          (ge3(s12, s23, s34, s14, s24, s13) * (d - 3) * _I4(1, 1, 1, 1, di + 2, s12, s23, s34, s14, s24, s13)
            - DerDet4x1(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, di, s34, s24, s23)
            - DerDet4x2(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, di, s34, s14, s13)
            - DerDet4x3(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, di, s24, s14, s12)
            - DerDet4x4(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, di, s23, s13, s12))

      if (di > 0)
        return ring(1) / (d - 5) / ge3(s12, s23, s34, s14, s24, s13) *
          (DerDet4x0(s12, s23, s34, s14, s24, s13) * _I4(1, 1, 1, 1, di - 2, s12, s23, s34, s14, s24, s13) * 2
            + DerDet4x1(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, di - 2, s34, s24, s23)
            + DerDet4x2(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, di - 2, s34, s14, s13)
            + DerDet4x3(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, di - 2, s24, s14, s12)
            + DerDet4x4(s12, s23, s34, s14, s24, s13) * _I3(1, 1, 1, di - 2, s23, s13, s12))
    }

    IntegralVal(iDef)
  }

  private[oneloop]
  def mkI3def(n1: Int, n2: Int, n3: Int, di: Int, s23: Expr, s13: Expr, s12: Expr) = {
    val d = dimension + di
    IntegralDef(threePointDef, Seq(n1, n2, n3), Seq(d, s23, s13, s12))
  }

  /** Computes 3-point massless integral, first looking into a cache */
  def I3(n1: Int, n2: Int, n3: Int, di: Int, s23: Expr, s13: Expr, s12: Expr, factorized: Boolean = false)
  : (IntegralDef[Expr], Result) = {
    // integral definition
    val iDef = mkI3def(n1, n2, n3, di, s23, s13, s12)
    // get from cache or compute from scratch
    (iDef, getOrCompute(iDef, () => __I3(iDef, n1, n2, n3, di, s23, s13, s12), factorized))
  }

  /** Computes 3-point massless integral, first looking into a cache */
  private
  def _I3(n1: Int, n2: Int, n3: Int, di: Int, s23: Expr, s13: Expr, s12: Expr)
  : IntegralVal[Expr] =
    I3(n1, n2, n3, di, s23, s13, s12, factorized = false)._2.left.get

  /** actual calculation of 3-point massless integral */
  private
  def __I3(iDef: IntegralDef[Expr], n1: Int, n2: Int, n3: Int, di: Int, s23: Expr, s13: Expr, s12: Expr): IntegralVal[Expr] = {
    assertInput(s23, s13, s12)

    if (n1 == 0)
      return _I2(n2, n3, di, s23)

    if (n2 == 0)
      return _I2(n1, n3, di, s13)

    if (n3 == 0)
      return _I2(n1, n2, di, s12)

    val d = dimension + di
    if ((n1, n2, n3) == (1, 1, 1) && di == 0) {
      if (ring.isZero(s12) && !(ring.isZero(s23) && ring.isZero(s13)))
        return ring(-2) * (d - 3) / (d - 4) / (s23 - s13) * (_I2(1, 1, di, s23) - _I2(1, 1, di, s13))

      if (ring.isZero(s13) && !(ring.isZero(s23) && ring.isZero(s12)))
        return ring(-2) * (d - 3) / (d - 4) / (s23 - s12) * (_I2(1, 1, di, s23) - _I2(1, 1, di, s12))

      if (ring.isZero(s23) && !(ring.isZero(s13) && ring.isZero(s12)))
        return ring(-2) * (d - 3) / (d - 4) / (s13 - s12) * (_I2(1, 1, di, s13) - _I2(1, 1, di, s12))
    }

    if (n1 > 1)
      return ring(1) / ge2(s23, s13, s12) / (n1 - 1) *
        (DerDet3x1(s23, s13, s12) * _I3(n1 - 1, n2, n3, di - 2, s23, s13, s12)
          + DerDet3x11(s23, s13, s12) * _I3(n1 - 2, n2, n3, di - 2, s23, s13, s12)
          + DerDet3x12(s23, s13, s12) * _I3(n1 - 1, n2 - 1, n3, di - 2, s23, s13, s12)
          + DerDet3x13(s23, s13, s12) * _I3(n1 - 1, n2, n3 - 1, di - 2, s23, s13, s12))

    if (n2 > 1)
      return ring(1) / ge2(s23, s13, s12) / (n2 - 1) * (DerDet3x2(s23, s13, s12)
        * _I3(n1, n2 - 1, n3, di - 2, s23, s13, s12)
        + DerDet3x21(s23, s13, s12) * _I3(n1 - 1, n2 - 1, n3, di - 2, s23, s13, s12)
        + DerDet3x22(s23, s13, s12) * _I3(n1, n2 - 2, n3, di - 2, s23, s13, s12)
        + DerDet3x23(s23, s13, s12) * _I3(n1, n2 - 1, n3 - 1, di - 2, s23, s13, s12))


    if (n3 > 1)
      return ring(1) / ge2(s23, s13, s12) / (n3 - 1) *
        (DerDet3x3(s23, s13, s12) * _I3(n1, n2, n3 - 1, di - 2, s23, s13, s12)
          + DerDet3x31(s23, s13, s12) * _I3(n1 - 1, n2, n3 - 1, di - 2, s23, s13, s12)
          + DerDet3x32(s23, s13, s12) * _I3(n1, n2 - 1, n3 - 1, di - 2, s23, s13, s12)
          + DerDet3x33(s23, s13, s12) * _I3(n1, n2, n3 - 2, di - 2, s23, s13, s12))

    if ((n1, n2, n3) == (1, 1, 1)) {
      if (di < 0)
        return ring(1) / 2 / DerDet3x0(s23, s13, s12) *
          ((d - 2) * ge2(s23, s13, s12) * _I3(1, 1, 1, di + 2, s23, s13, s12)
            - DerDet3x1(s23, s13, s12) * _I2(1, 1, di, s23)
            - DerDet3x2(s23, s13, s12) * _I2(1, 1, di, s13)
            - DerDet3x3(s23, s13, s12) * _I2(1, 1, di, s12))

      if (di > 0)
        return ring(1) / (d - 4) / ge2(s23, s13, s12) * (
          DerDet3x0(s23, s13, s12) * _I3(1, 1, 1, di - 2, s23, s13, s12) * 2
            + DerDet3x1(s23, s13, s12) * _I2(1, 1, di - 2, s23)
            + DerDet3x2(s23, s13, s12) * _I2(1, 1, di - 2, s13)
            + DerDet3x3(s23, s13, s12) * _I2(1, 1, di - 2, s12))
    }

    IntegralVal(iDef)
  }

  private[oneloop]
  def mkI2def(n1: Int, n2: Int, di: Int, s12: Expr) = {
    // integral definition
    val d = dimension + di
    IntegralDef(twoPointDef, Seq(n1, n2), Seq(d, s12))
  }

  /** Computes 2-point massless integral, first looking into a cache */
  def I2(n1: Int, n2: Int, di: Int, s12: Expr, factorized: Boolean = false)
  : (IntegralDef[Expr], Result) = {
    // integral definition
    val iDef = mkI2def(n1, n2, di, s12)
    // get from cache or compute from scratch
    (iDef, getOrCompute(iDef, () => __I2(n1, n2, di, s12), factorized))
  }

  /** Computes 2-point massless integral, first looking into a cache */
  private
  def _I2(n1: Int, n2: Int, di: Int, s12: Expr)
  : IntegralVal[Expr] =
    I2(n1, n2, di, s12, factorized = false)._2.left.get

  /** Two-point integral */
  def __I2(n1: Int, n2: Int, di: Int, s12: Expr): IntegralVal[Expr] = {
    assertInput(s12)

    if (s12.isZero || n1 <= 0 || n2 <= 0)
      return IntegralVal.zero
    val d = dimension
    ring(-1).pow(di / 2) * s12.pow(di / 2 + 2 - n1 - n2) *
      Pochhammer(d / 2 - 1, 1 + di / 2 - n1) *
      Pochhammer(d / 2 - 1, 1 + di / 2 - n2) *
      Pochhammer(2 - d / 2, n1 + n2 - di / 2 - 2) /
      Pochhammer(d - 2, 2 + di - n1 - n2) /
      ring.factorial(n1 - 1) /
      ring.factorial(n2 - 1) *
      IntegralVal(twoPointDef, Seq(1, 1), Seq(d, s12))
  }

  private
  def Pochhammer(q: Expr, n: Int): Expr = n match {
    case 0 => ring(1)
    case _ if n > 0 => ring.multiply((0 until n).map(q + _): _*)
    case _ if n < 0 => ring(1) / Pochhammer(q + n, -n)
  }

  //////////////////////////////////// Tensor integrals ////////////////////////////////////

  type tProduct = cc.redberry.core.tensor.Product
  type tSum = cc.redberry.core.tensor.Sum
  type TensorResult = Map[Indexed, Result]

  implicit class TensorIntegralValOps(terms: TensorResult) extends PrintableOps {
    def print(stream: PrintStream, of: PrintFormatter): Unit = {
      if (terms.isEmpty)
        stream.print("0")
      else
        terms.toSeq.foldLeft(0) { case (i, (tensor, scalar)) =>
          if (i != 0 || of.tablePrint)
            stream.print(" + ")
          if (tensor.tensor.isInstanceOf[tSum]) {
            stream.print("(")
            stream.print(tensor.tensor)
            stream.print(")")
          } else
            stream.print(tensor)

          stream.print(" * ")
          stream.print("(")
          scalar.print(stream, of)
          stream.print(")")
          if (of.tablePrint)
            stream.print("\n")
          i + 1
        }
    }
  }

  case class Indexed(tensor: Tensor) {
    override def hashCode(): Int = tensor.hashCode()

    override def equals(obj: Any): Boolean =
      obj.isInstanceOf[Indexed] && TensorUtils.equals(tensor, obj.asInstanceOf[Indexed].tensor)

    override def toString: String = tensor.toString
  }

  /**
    * Evaluates oneloop tensor massless integral
    *
    * @param integral  integral
    * @param indices   tensor indices
    * @param factorize whether to factorize the result
    * @return
    */
  def evaluate(integral: IntegralDef[Expr], indices: String, factorize: Boolean): TensorResult
  = evaluate(integral, ParserIndices.parseSimpleIgnoringVariance(s"^{$indices}").toArray.map(IndicesUtils.toString).toSeq, factorize)

  /**
    * Evaluates oneloop tensor massless integral
    *
    * @param integral  integral
    * @param indices   tensor indices
    * @param factorize whether to factorize the result
    * @return
    */
  def evaluate(integral: IntegralDef[Expr], indices: Seq[String], factorize: Boolean)
  : TensorResult = {
    val formula = Util.mkTensorReduction(integral.indices.length, indices)
    log(s"Tensor reduction formula: $formula")
    applyTensorReduction(formula, integral, factorize)
  }

  private
  def applyTensorReduction(formula: Tensor,
                           integral: IntegralDef[Expr],
                           factorize: Boolean)
  = {
    val terms: Seq[Tensor] =
      if (formula.isInstanceOf[tSum])
        formula.asScala.toSeq
      else
        Seq(formula)

    terms.map { term =>
      val pTerm = term.asInstanceOf[tProduct]
      val (tPart, sPart) =
        if (!pTerm.getFactor.isImaginary)
          (pTerm.getDataSubProduct, pTerm.getIndexlessSubProduct)
        else
          (Tensors.multiply(pTerm.getDataSubProduct, pTerm.getFactor),
            pTerm.getSubProductWithoutFactor.asInstanceOf[tProduct].getIndexlessSubProduct)

      val sPartTerms =
        if (sPart.isInstanceOf[tSum])
          sPart.asScala.toSeq
        else
          Seq(sPart)

      log(s"Computing scalar coefficient for ($tPart) structure of $integral")
      val results = sPartTerms.map(applyScalarTensorReductionP(_, integral, factorize)).filter(_ != null)
      (Indexed(tPart), results.fold(results.head.zero()) { _ + _ })
    }.toMap
  }

  private
  def applyScalarTensorReductionP(term: Tensor,
                                  integral: IntegralDef[Expr],
                                  factorize: Boolean): Result
  = {
    assert(TensorUtils.isIndexless(term), term)

    var pTerm = term

    // dimension shift
    val dExponent = TensorUtils.Exponent(term, parse("d"))
    pTerm = expression(parse("d"), parse("1"))
      .transform(pTerm)


    // dimension due to mass derivatives
    var massDcf = ring(1)
    val massDexps = mutable.Map.empty[Int, Int]
    for (i <- integral.indices.indices) {
      val al = parse(s"alpha[$i]")
      massDexps(i) = TensorUtils.Exponent(term, al)
      if (massDexps(i) != 0) {
        if (integral.indices(i) == 0)
          return null

        for (e <- 0 until massDexps(i))
          massDcf *= integral.indices(i) + e

        pTerm = expression(al, parse("1"))
          .transform(pTerm)
      }
    }

    val cf = ring(pTerm
      .toString(OutputFormat.Maple)
      .replace("**", "^")) * massDcf

    val shiftedIntegral =
      integral.copy(
        args = integral.args.updated(0, integral.args(0) + (dExponent * 2)),
        indices = integral.indices.zipWithIndex.map { case (e, i) => e + massDexps.getOrElse(i, 0) })

    log(s"Computing shifted integral $shiftedIntegral of $integral")
    evaluate(shiftedIntegral, factorize)._2 * cf
  }
}

object Util {
  /** create tensor reduction formula */
  def mkTensorReduction(nPropagators: Int, indices: Seq[String]): Tensor = {
    var expArg = parse("0")
    for (i <- 0 until (nPropagators - 1)) {
      expArg = sum(expArg, parse(s"a_{m}*p^{m} * alpha[$i]"))
    }
    expArg = subtract(expArg, parse("a_{m}*a^{m} / 4"))
    val tOp = parse(s"Exp[I * ($expArg) * d]")

    var diff = tOp
    for (i <- indices) {
      val dvar = parseSimple(s"a^$i")
      diff = differentiate(diff, dvar)
    }

    val zSub = expression(parse("a_i"), parse("0"))
    val res = zSub.transform(diff)
    res
  }

  /** auxiliary method used to shuffle variables (from <-> to) */
  private
  def replaceVariables[E](expr: MultivariatePolynomial[E],
                          from_to: Seq[(MultivariatePolynomial[E], MultivariatePolynomial[E])])
  : MultivariatePolynomial[E] = {

    val iMap = from_to.map(t => (t._1.univariateVariable(), if (t._2.isZero) -1 else t._2.univariateVariable())).toMap
    assert(expr.degrees().zipWithIndex.forall { case (d, i) => iMap.contains(i) || d == 0 })

    expr.mapTerms(expr.ring, term => {
      val oldExponents = term.exponents
      val newExponents = Array.ofDim[Int](oldExponents.length)

      iMap.foreach { case (f, t) =>
        if (t != -1) newExponents(t) = oldExponents(f)
      }

      term.setDegreeVector(newExponents)
    })
  }

  /** auxiliary method used to shuffle variables (from <-> to) */
  def replaceVariables[E](expr: Rational[MultivariatePolynomial[E]],
                          from: Seq[Rational[MultivariatePolynomial[E]]],
                          to: Seq[Rational[MultivariatePolynomial[E]]])
  : Rational[MultivariatePolynomial[E]] = {
    var _from = from.map(_.numerator())
    var _to = to.map(_.numerator())

    val substitutions = _from zip _to
    if (substitutions.exists(t => t._1.isZero && !t._2.isZero))
      throw new RuntimeException

    expr.map((f: MultivariatePolynomial[E]) => replaceVariables(f, substitutions.filterNot(_._1.isZero)))
  }
}