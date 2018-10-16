package cc.redberry.rings.oneloop


import cc.redberry.rings.oneloop.Definitions.{FactorizedIntegralVal, IntegralVal, PrintFormat, PrintFormatter}
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
      descr = "Path to database file",
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
      descr = s"Format of output. Possible values: ${PrintFormat.values.map(_.toString).mkString(",")}. Default is ${PrintFormat.MMA}.",
      default = Some("MMA"),
      required = false,
      validate = s => PrintFormat.byName(s).isDefined)

    val tablePrint = opt[Boolean](
      name = "table-print",
      descr = s"Print each summand of the result on a new line.",
      default = Some(false),
      required = false)
  }

  trait ThreePointOpts extends GenericOpts {
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

    val n3 = opt[Int](
      descr = "Exponent of the third propagator",
      default = None,
      required = true,
      noshort = true,
      validate = _ >= 0)
  }

  trait FourPointOpts extends ThreePointOpts {
    this: ScallopConfBase =>
    val n4 = opt[Int](
      descr = "Exponent of the fourth propagator",
      default = None,
      required = true,
      noshort = true,
      validate = _ >= 0)
  }

  trait FivePointOpts extends FourPointOpts {
    this: ScallopConfBase =>
    val n5 = opt[Int](
      descr = "Exponent of the fifth propagator",
      default = None,
      required = true,
      noshort = true,
      validate = _ >= 0)
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
         |Usage: $ProgramName [i3|i4|i5] [--d <shift>] --n1 <n1> --n2 <n2>  ...
         |Options:""".stripMargin)

    val threePoint = new Subcommand("i3") with ThreePointOpts {
      mainOptions = Seq(n1, n2, n3, di)
    }
    addSubcommand(threePoint)

    val fourPoint = new Subcommand("i4") with FourPointOpts {
      mainOptions = Seq(n1, n2, n3, n4, di)
    }
    addSubcommand(fourPoint)

    val fivePoint = new Subcommand("i5") with FivePointOpts {
      mainOptions = Seq(n1, n2, n3, n4, n5, di)
    }

    addSubcommand(fivePoint)

    // disable help for subcommands
    helpFormatter = new ScallopHelpFormatter() {
      override def formatHelp(s: Scallop, subcommandPrefix: String): String = super.formatHelp(fivePoint.builder, subcommandPrefix)

      override protected def getSubcommandsHelp(s: Scallop, subcommandPrefix: String): String = ""
    }

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

    val cfRing = if (char == 0) Z else Zp(char)
    val calculator = new MasslessIntegrals(cfRing, databaseFile = if (genOpts.noDB()) None else Some(genOpts.dbFile()))
    implicit val ring: Frac[MultivariatePolynomial[IntZ]] = calculator.ring
    import calculator.momentums._

    try {
      val integral: Either[IntegralVal[calculator.Expr], FactorizedIntegralVal[calculator.Expr]] = conf.subcommands match {
        case (tp@conf.threePoint) :: Nil =>
          calculator.I3(tp.n1(), tp.n2(), tp.n3(), tp.di(), s23, s13, s12, tp.factorize())
        case (fp@conf.fourPoint) :: Nil =>
          calculator.I4(fp.n1(), fp.n2(), fp.n3(), fp.n4(), fp.di(), s12, s23, s34, s14, s24, s13, fp.factorize())
        case (fp@conf.fivePoint) :: Nil =>
          calculator.I5(fp.n1(), fp.n2(), fp.n3(), fp.n4(), fp.n5(), fp.di(), s12, s23, s34, s45, s15, s13, s14, s24, s25, s35, fp.factorize())
        case _ =>
          helpAndReturn()
          null
      }

      integral match {
        case Left(v) => v.print(System.out, formatter)
        case Right(v) => v.print(System.out, formatter)
      }
      System.out.print("\n")

    } finally
      calculator.close()
  }
}