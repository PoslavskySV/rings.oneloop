package cc.redberry.rings.oneloop


import cc.redberry.rings.oneloop.Definitions.{FactorizedIntegralVal, IntegralDef, IntegralVal, PrintFormat, PrintFormatter}
import cc.redberry.rings.scaladsl._
import org.rogach.scallop._

/**
  *
  */
object Main {
  val ProgramName = "oneloop"

  trait GenericOpts {
    this: ScallopConfBase =>

    val characteristic = opt[Long](
      name = "characteristic",
      descr = "Characteristic of the used ring",
      default = Some(0),
      required = false
    )

    val factorize = opt[Boolean](
      name = "factorize",
      default = Some(false),
      descr = "Factorize coefficient at each integral summand",
      required = false)

    val dbFile = opt[String](
      name = "database",
      descr = "Alternative path to database file",
      default = Some("integrals.db"),
      required = false,
      noshort = true)

    val noDB = opt[Boolean](
      name = "no-database",
      default = Some(false),
      descr = "Disable use of database (each expression will be calculated from scratch)",
      required = false,
      noshort = true
    )

    val outputFormat = opt[String](
      name = "output-format",
      descr = s"Format of output. Possible values: ${PrintFormat.values.map(_.toString).mkString(", ")}. Default is ${PrintFormat.MMA}.",
      default = Some(PrintFormat.MMA.toString),
      required = false,
      validate = s => PrintFormat.byName(s).isDefined)

    val tablePrint = opt[Boolean](
      name = "table-print",
      descr = "Print each summand of the result on a new line",
      default = Some(false),
      required = false)

    def kinematicInvariants(): Seq[String] = Seq.empty
  }

  trait TwoPointOpts extends GenericOpts {
    this: ScallopConfBase =>

    val di = opt[Int](
      descr = "Dimension shift (must be even)",
      default = Some(0),
      required = false,
      noshort = true,
      validate = _ % 2 == 0
    )

    val n1 = opt[Int](
      descr = "Exponent of the first propagator",
      default = None,
      required = true,
      noshort = true,
      validate = _ >= 0)

    val n2 = opt[Int](
      descr = "Exponent of the second propagator",
      default = None,
      required = true,
      noshort = true,
      validate = _ >= 0)

    val s12 = opt[String](
      descr = "Optional value for kinematic invariant s12",
      default = Some("s12"),
      required = false,
      noshort = true)

    override def kinematicInvariants(): Seq[String] = super.kinematicInvariants() ++ Seq(s12())
  }

  trait ThreePointOpts extends TwoPointOpts {
    this: ScallopConfBase =>

    val n3 = opt[Int](
      descr = "Exponent of the third propagator",
      default = None,
      required = true,
      noshort = true,
      validate = _ >= 0)

    val s13 = opt[String](
      descr = "Optional value for kinematic invariant s13",
      default = Some("s13"),
      required = false,
      noshort = true)

    val s23 = opt[String](
      descr = "Optional value for kinematic invariant s23",
      default = Some("s23"),
      required = false,
      noshort = true)

    override def kinematicInvariants(): Seq[String] = super.kinematicInvariants() ++ Seq(s13(), s23())
  }

  trait FourPointOpts extends ThreePointOpts {
    this: ScallopConfBase =>
    val n4 = opt[Int](
      descr = "Exponent of the fourth propagator",
      default = None,
      required = true,
      noshort = true,
      validate = _ >= 0)

    val s14 = opt[String](
      descr = "Optional value for kinematic invariant s14",
      default = Some("s14"),
      required = false,
      noshort = true)

    val s24 = opt[String](
      descr = "Optional value for kinematic invariant s24",
      default = Some("s24"),
      required = false,
      noshort = true)

    val s34 = opt[String](
      descr = "Optional value for kinematic invariant s24",
      default = Some("s34"),
      required = false,
      noshort = true)

    override def kinematicInvariants(): Seq[String] = super.kinematicInvariants() ++ Seq(s14(), s24(), s34())
  }

  trait FivePointOpts extends FourPointOpts {
    this: ScallopConfBase =>
    val n5 = opt[Int](
      descr = "Exponent of the fifth propagator",
      default = None,
      required = true,
      noshort = true,
      validate = _ >= 0)

    val s15 = opt[String](
      descr = "Optional value for kinematic invariant s15",
      default = Some("s15"),
      required = false,
      noshort = true)

    val s25 = opt[String](
      descr = "Optional value for kinematic invariant s25",
      default = Some("s25"),
      required = false,
      noshort = true)

    val s35 = opt[String](
      descr = "Optional value for kinematic invariant s25",
      default = Some("s35"),
      required = false,
      noshort = true)

    val s45 = opt[String](
      descr = "Optional value for kinematic invariant s25",
      default = Some("s45"),
      required = false,
      noshort = true)

    override def kinematicInvariants(): Seq[String] = super.kinematicInvariants() ++ Seq(s15(), s25(), s35(), s45())
  }

  /**
    * Global command line configuration
    *
    * @param arguments
    */
  class GlobalConf(arguments: Seq[String]) extends ScallopConf(arguments) {
    version(s"$ProgramName v1.0")
    banner(
      s"""
         |Usage: $ProgramName i2|i3|i4|i5 [--di <shift>] --n1 <n1> --n2 <n2>  ...
         |Options:""".stripMargin)

    val twoPoint = new Subcommand("i2") with TwoPointOpts {
      descr("Computes massless 2-point integral I2[s12] in (d + di) dimensions")
      mainOptions = Seq(n1, n2, di)
    }
    addSubcommand(twoPoint)

    val threePoint = new Subcommand("i3") with ThreePointOpts {
      descr("Computes massless 3-point integral I3[s23, s13, s12] in (d + di) dimensions")
      mainOptions = Seq(n1, n2, n3, di)
    }
    addSubcommand(threePoint)

    val fourPoint = new Subcommand("i4") with FourPointOpts {
      descr("Computes massless 4-point integral I4[s12, s23, s34, s14, s24, s13] in (d + di) dimensions")
      mainOptions = Seq(n1, n2, n3, n4, di)
    }
    addSubcommand(fourPoint)

    val fivePoint = new Subcommand("i5") with FivePointOpts {
      descr("Computes massless 5-point integral I5[s12, s23, s34, s45, s15, s13, s14, s24, s25, s35] in (d + di) dimensions")
      mainOptions = Seq(n1, n2, n3, n4, n5, di)
    }
    addSubcommand(fivePoint)

    verify()
  }

  def main(args: Array[String]): Unit = {
    val conf = new GlobalConf(args)

    def helpAndReturn(header: String = "", exitCode: Int = 0): Unit = {
      println(
        s"""
           | $header
           | ${conf.printHelp()}
      """.stripMargin)
      System.exit(exitCode)
    }

    if (conf.subcommands.isEmpty)
      helpAndReturn()

    val genOpts: GenericOpts = conf.subcommand match {
      case Some(cmd) => cmd.asInstanceOf[GenericOpts]
      case None => helpAndReturn(); null
    }

    val char = genOpts.characteristic()
    val formatter = PrintFormatter(PrintFormat.byName(genOpts.outputFormat()).get, genOpts.tablePrint())
    val usedVariables = genOpts
      .kinematicInvariants()
      .mkString(" ")
      .replaceAll("[\\+\\-\\*/\\^\\(\\)]", " ")
      .replaceAll("\\s+", " ")
      .split(" ")
      .filterNot(_.isEmpty)
      .filterNot(_.matches("[0-9]+"))
      .distinct
      .sorted

    val variables =
      if (genOpts.kinematicInvariants().size > usedVariables.length)
        usedVariables ++ (0 until (genOpts.kinematicInvariants().size - usedVariables.length)).map(i => "dummyVar" + i)
      else
        usedVariables

    val cfRing = if (char == 0) Z else Zp(char)
    val calculator = new MasslessIntegrals(cfRing,
      databaseFile = if (genOpts.noDB()) None else Some(genOpts.dbFile()),
      usedVariables = variables)
    implicit val ring: Frac[MultivariatePolynomial[IntZ]] = calculator.ring
    import calculator.Expr

    val start = System.nanoTime()
    try {
      val (iDef, iVal): (IntegralDef[Expr], Either[IntegralVal[Expr], FactorizedIntegralVal[Expr]])
      = conf.subcommands match {
        case (tp@conf.twoPoint) :: Nil =>
          calculator.I2(tp.n1(), tp.n2(), tp.di(), ring(tp.s12()), tp.factorize())
        case (tp@conf.threePoint) :: Nil =>
          calculator.I3(tp.n1(), tp.n2(), tp.n3(), tp.di(), ring(tp.s23()), ring(tp.s13()), ring(tp.s12()), tp.factorize())
        case (fp@conf.fourPoint) :: Nil =>
          calculator.I4(fp.n1(), fp.n2(), fp.n3(), fp.n4(), fp.di(),
            ring(fp.s12()), ring(fp.s23()), ring(fp.s34()), ring(fp.s14()), ring(fp.s24()), ring(fp.s13()), fp.factorize())
        case (fp@conf.fivePoint) :: Nil =>
          calculator.I5(fp.n1(), fp.n2(), fp.n3(), fp.n4(), fp.n5(), fp.di(),
            ring(fp.s12()), ring(fp.s23()), ring(fp.s34()), ring(fp.s45()), ring(fp.s15()),
            ring(fp.s13()), ring(fp.s14()), ring(fp.s24()), ring(fp.s25()), ring(fp.s35()),
            fp.factorize())
        case _ =>
          helpAndReturn()
          null
      }
      val elapsed = System.nanoTime() - start

      import scala.concurrent.duration._

      System.err.println(s"Finished in ${elapsed.nanos.toSeconds}s")

      iDef.print(System.out, formatter)
      if (formatter.fmt == PrintFormat.Maple)
        System.out.print(s" := ")
      else
        System.out.print(s" = ")

      if (formatter.tablePrint)
        System.out.print("\n")

      iVal match {
        case Left(v) => v.print(System.out, formatter)
        case Right(v) => v.print(System.out, formatter)
      }
      System.out.print("\n")

    } finally
      calculator.close()
  }
}