/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package edu.cmu.cs.ls.keymaerax.cli

import edu.cmu.cs.ls.keymaerax.btactics.ModelPlexKind
import edu.cmu.cs.ls.keymaerax.info.{FullCopyright, FullNameAndVersion, License, ShortCopyright, ThirdPartyLicenses}
import edu.cmu.cs.ls.keymaerax.tools.ToolName
import edu.cmu.cs.ls.keymaerax.tools.install.ToolConfiguration
import scopt.OParser

import scala.concurrent.duration.Duration

// TODO Convert to Scala 3 enum
object Conversion extends Enumeration {
  val StripHints: Value = Value
  val KyxToMat: Value = Value
  val KyxToSmt: Value = Value
  val SmtToKyx: Value = Value
  val SmtToMat: Value = Value
}

// TODO Convert to Scala 3 enum
object TacticConversion extends Enumeration {
  val Verbose, Labelled, Verbatim = Value
}

// TODO Convert to Scala 3 enum
sealed trait Command
object Command {
  // Core commands
  case object Setup extends Command
  case class Prove(
      in: String = null,
      out: Option[String] = None,
      ptOut: Option[String] = None,
      conjecture: Option[String] = None,
      tactic: Option[String] = None,
      tacticName: Option[String] = None,
      timeout: Duration = Duration.Inf,
      verbose: Boolean = false,
      statistics: KeYmaeraXProofChecker.StatisticsPrinter.Value = KeYmaeraXProofChecker.StatisticsPrinter.Csv,
  ) extends Command
  case class Parse(in: String = null) extends Command
  case class BParse(in: String = null) extends Command
  case class Convert(conversion: Conversion.Value = null, in: String = null, out: Option[String] = None) extends Command
  case class Grade(
      in: String = null,
      out: Option[String] = None,
      exportAnswers: Boolean = false,
      skipOnParseError: Boolean = false,
  ) extends Command
  // Webui commands
  case class Codegen(
      in: String = null,
      out: Option[String] = None,
      quantitative: Boolean = false,
      interval: Boolean = true,
      vars: Option[Seq[String]] = None,
  ) extends Command
  case class ConvertTactics(conversion: TacticConversion.Value = null, in: String = null, out: String = null)
      extends Command
  case class Modelplex(
      in: String = null,
      out: Option[String] = None,
      ptOut: Option[String] = None,
      vars: Option[Seq[String]] = None,
      verify: Boolean = false,
      sandbox: Boolean = false,
      monitor: Option[ModelPlexKind.Value] = None,
      fallback: Option[String] = None,
  ) extends Command
  case class Repl(model: String = null, tactic: Option[String] = None, scaladefs: Option[String] = None) extends Command
  case object Ui extends Command
}

// TODO Turn into Scala 3 enum
object JlinkInterface extends Enumeration {
  val String, Expr = Value

  def description(value: Value): String = value match {
    case String => "more robust"
    case Expr => "supports larger queries"
  }
}

// TODO Turn into Scala 3 enum
object QeMethod extends Enumeration {
  val Reduce, Resolve = Value

  def description(value: Value): String = value match {
    case Reduce => "solves equations and eliminates quantifiers"
    case Resolve => "eliminates quantifiers"
  }
}

case class Options(
    // Special options
    name: String,
    args: Seq[String],
    help: Boolean = false,
    copyright: Boolean = false,
    license: Boolean = false,
    thirdPartyLicenses: Boolean = false,
    launch: Boolean = false,
    command: Option[Command] = None,
    // Options specified using flags
    debug: Option[Boolean] = None,
    jlink: Option[String] = None,
    jlinkinterface: Option[JlinkInterface.Value] = None,
    jlinktcpip: Option[Boolean] = None,
    lax: Option[Boolean] = None,
    mathkernel: Option[String] = None,
    open: Option[String] = None,
    parallelqe: Option[Boolean] = None,
    parserClass: Option[String] = None,
    qemethod: Option[QeMethod.Value] = None,
    tool: Option[ToolName.Value] = None,
    z3Path: Option[String] = None,
) {

  /** Helper function to make updating command-specific options easier. */
  private def updateCommand[C <: Command](update: C => C): Options = this
    .copy(command = Some(update(this.command.get.asInstanceOf[C])))

  def toToolConfig: ToolConfiguration = ToolConfiguration(
    tool = this.tool,
    mathKernel = this.mathkernel,
    jlinkLibDir = this.jlink,
    tcpip = None,
    z3Path = this.z3Path,
  )

  def printUsageAndExitWithError(): Unit = {
    println(OParser.usage(Options.parser(name)))
    sys.exit(1)
  }
}

object Options {
  private val CopyrightFlagName = "copyright"
  private val LicenseFlagName = "license"
  private val ThirdPartyLicensesFlagName = "third-party-licenses"
  private val LaunchFlagName = "launch"
  val LaunchFlag = s"--$LaunchFlagName"

  private def wrapWide(text: String): String = TextWrapper.wrap(text, maxWidth = 80)

  private def wrap(text: String): String = {
    // The left column of the help output (including the space separating the columns) has a width of 27 characters.
    TextWrapper.wrap(text, maxWidth = 80 - 27)
  }

  private def valueToString[T](value: T): String = value match {
    // Without this special case, Duration.Inf would be printed as "Default: Duration.Inf".
    // This does not match the duration help text, which suggests "Inf".
    case Duration.Inf => "Inf"
    case v => v.toString
  }

  private def possibleValues[T](values: Iterable[T]): String =
    s"Possible values: ${values.map(valueToString).mkString(", ")}."

  private def possibleValuesWithDescriptions[T](values: Iterable[T], description: T => String): String = {
    val describedValues = values.map(v => s"${valueToString(v)} (${description(v)})")
    s"Possible values: ${describedValues.mkString(", ")}."
  }

  private def defaultValue[T](value: T): String = s"Default: ${valueToString(value)}."

  private def parser(name: String): OParser[Unit, Options] = {
    val builder = OParser.builder[Options]
    import builder._

    implicit val conversionRead: scopt.Read[Conversion.Value] = scopt.Read.reads(Conversion.withName)

    implicit val jlinkInterfaceRead: scopt.Read[JlinkInterface.Value] = scopt.Read.reads(JlinkInterface.withName)

    implicit val keYmaeraXProofCheckerStatisticsPrinterRead: scopt.Read[KeYmaeraXProofChecker.StatisticsPrinter.Value] =
      scopt.Read.reads(KeYmaeraXProofChecker.StatisticsPrinter.withName)

    implicit val modelPlexKindRead: scopt.Read[ModelPlexKind.Value] = scopt.Read.reads(ModelPlexKind.withName)

    implicit val qeMethodRead: scopt.Read[QeMethod.Value] = scopt.Read.reads(QeMethod.withName)

    implicit val tacticConversionRead: scopt.Read[TacticConversion.Value] = scopt.Read.reads(TacticConversion.withName)

    implicit val toolNameRead: scopt.Read[ToolName.Value] = scopt.Read.reads(ToolName.withName)

    OParser.sequence(
      programName(name),
      head(
        s"""$FullNameAndVersion
           |$ShortCopyright
           |See --$CopyrightFlagName, --$LicenseFlagName, and --$ThirdPartyLicensesFlagName for more details.
           |""".stripMargin
      ),
      opt[Unit]('h', "help").action((_, o) => o.copy(help = true)).text(wrap("Show this usage information.")),
      opt[Unit](CopyrightFlagName).hidden().action((_, o) => o.copy(copyright = true)),
      opt[Unit](LicenseFlagName).hidden().action((_, o) => o.copy(license = true)),
      opt[Unit](ThirdPartyLicensesFlagName).hidden().action((_, o) => o.copy(thirdPartyLicenses = true)),
      opt[Unit](LaunchFlagName)
        .action((_, o) => o.copy(launch = true))
        .text(wrap("Use present JVM instead of launching one with a bigger stack.")),

      // Options specified using flags
      opt[Boolean]("debug")
        .action((x, o) => o.copy(debug = Some(x)))
        .text(wrap("Enable/disable debug mode with exhaustive messages.")),
      opt[String]("jlink")
        .action((x, o) => o.copy(jlink = Some(x)))
        .valueName("path/to/jlinkNativeLib")
        .text(wrap("Path to Mathematica J/Link library directory.")),
      opt[JlinkInterface.Value]("jlinkinterface")
        .action((x, o) => o.copy(jlinkinterface = Some(x)))
        .valueName("<format>")
        .text(wrap(
          s"""Format for sending commands to Mathematica.
             |${possibleValuesWithDescriptions(JlinkInterface.values, JlinkInterface.description)}
             |""".stripMargin
        )),
      opt[Boolean]("jlinktcpip")
        .action((x, o) => o.copy(jlinktcpip = Some(x)))
        .valueName("<bool>")
        .text(wrap(
          """Whether to connect to Mathematica with
            |process communication or
            |over TCP/IP (more robust).
            |Default: false (unless configured in keymaerax.conf)
            |""".stripMargin
        )),
      opt[Boolean]("lax")
        .action((x, o) => o.copy(lax = Some(x)))
        .text(wrap(
          """true: Use lax mode with more flexible parser, printer, prover.
            |false: Use strict mode with no flexibility in prover.
            |""".stripMargin
        )),
      opt[String]("mathkernel")
        .action((x, o) => o.copy(mathkernel = Some(x)))
        .valueName("MathKernel(.exe)")
        .text(wrap("Path to Mathematica kernel executable.")),
      opt[String]("open").action((x, o) => o.copy(open = Some(x))),
      opt[Boolean]("parallelqe")
        .action((x, o) => o.copy(parallelqe = Some(x)))
        .valueName("<bool>")
        .text(wrap(
          """Whether to attempt multiple QE alternatives in parallel.
            |Default: false (unless configured in keymaerax.conf)
            |""".stripMargin
        )),
      opt[String]("parserClass").action((x, o) => o.copy(parserClass = Some(x))),
      opt[QeMethod.Value]("qemethod")
        .action((x, o) => o.copy(qemethod = Some(x)))
        .valueName("<method>")
        .text(wrap(
          s"""Quantifier elimination method to use.
             |${possibleValuesWithDescriptions(QeMethod.values, QeMethod.description)}
             |""".stripMargin
        )),
      opt[ToolName.Value]("tool")
        .action((x, o) => o.copy(tool = Some(x)))
        .valueName("<name>")
        .text(wrap(
          s"""Choose which tool to use for real arithmetic.
             |${possibleValues(ToolName.values)}
             |""".stripMargin
        )),
      opt[String]("z3path")
        .action((x, o) => o.copy(z3Path = Some(x)))
        .valueName("path/to/z3")
        .text(wrap("Path to Z3 executable.")),

      // Core commands
      note(""),
      cmd("setup")
        .action((_, o) => o.copy(command = Some(Command.Setup)))
        .text(wrapWide("Initialize the configuration and lemma cache.")),
      note(""),
      cmd("prove")
        .action((_, o) => o.copy(command = Some(Command.Prove())))
        .text(wrapWide("Run prover on given archive of models or proofs."))
        .children(
          arg[String]("<in>")
            .action((x, o) => o.updateCommand[Command.Prove](_.copy(in = x)))
            .text(wrap("Input archive file(s) (either a specific file or a wildcard, e.g. *.kyx).")),
          arg[String]("<out>")
            .optional()
            .action((x, o) => o.updateCommand[Command.Prove](_.copy(out = Some(x))))
            .text(wrap("Output proof file (defaults to input file with .kyp suffix).")),
          opt[String]("pt-out")
            .action((x, o) => o.updateCommand[Command.Prove](_.copy(ptOut = Some(x))))
            .valueName("<file>")
            .text(wrap("Output proof term s-expression into a file.")),
          opt[String]("conjecture")
            .action((x, o) => o.updateCommand[Command.Prove](_.copy(ptOut = Some(x))))
            .valueName("<file>")
            .text(wrap(
              """Conjecture file to replace the model in the input file with
                |(either a specific file or a wildcard, e.g. *.kyx).
                |""".stripMargin
            )),
          opt[String]("tactic")
            .action((x, o) => o.updateCommand[Command.Prove](_.copy(tactic = Some(x))))
            .valueName("<file|tactic>")
            .text(wrap(
              """Either a file containing a tactic, or a parseable tactic.
                |Used to prove the entry/entries in the input or conjecture file.
                |""".stripMargin
            )),
          opt[String]("tactic-name")
            .action((x, o) => o.updateCommand[Command.Prove](_.copy(tacticName = Some(x))))
            .valueName("<name>")
            .text(wrap(
              """Which of the tactics in the input file to use
                |(default: check all, falling back to auto if no tactic is listed).
                |Only used if no tactic is specified.
                |""".stripMargin
            )),
          opt[Duration]("timeout")
            .action((x, o) => o.updateCommand[Command.Prove](_.copy(timeout = x)))
            .validate(d => if (d > Duration.Zero) success else failure("timeout must be positive"))
            .valueName("<duration>")
            .text(wrap(
              s"""How long to try proving before giving up.
                 |Format: "<length><unit>" or "Inf".
                 |Available units: d (day), h (hour), m (minute), s (second), ms (millisecond).
                 |${defaultValue(Command.Prove().timeout)}
                 |""".stripMargin
            )),
          opt[Unit]("verbose")
            .action((_, o) => o.updateCommand[Command.Prove](_.copy(verbose = true)))
            .text(wrap("Print verbose proof information.")),
          opt[KeYmaeraXProofChecker.StatisticsPrinter.Value]("statistics")
            .action((x, o) => o.updateCommand[Command.Prove](_.copy(statistics = x)))
            .valueName("<format>")
            .text(wrap(
              s"""How to print proof statistics.
                 |${possibleValues(KeYmaeraXProofChecker.StatisticsPrinter.values)}
                 |${defaultValue(Command.Prove().statistics)}
                 |""".stripMargin
            )),
        ),
      note(""),
      cmd("parse")
        .action((_, o) => o.copy(command = Some(Command.Parse())))
        .text(wrapWide("Return error code 0 if the given model file parses."))
        .children(
          arg[String]("<in>").action((x, o) => o.updateCommand[Command.Parse](_.copy(in = x))).text(wrap("Input file."))
        ),
      note(""),
      cmd("bparse")
        .action((_, o) => o.copy(command = Some(Command.BParse())))
        .text(wrapWide("Return error code 0 if given bellerophon tactic file parses."))
        .children(
          arg[String]("<in>")
            .action((x, o) => o.updateCommand[Command.BParse](_.copy(in = x)))
            .text(wrap("Input file."))
        ),
      note(""),
      cmd("convert")
        .action((_, o) => o.copy(command = Some(Command.Convert())))
        .text(wrapWide("Model conversions."))
        .children(
          arg[Conversion.Value]("<conversion>")
            .action((x, o) => o.updateCommand[Command.Convert](_.copy(conversion = x)))
            .text(wrap(
              s"""Conversion to perform.
                 |${possibleValues(Conversion.values)}
                 |""".stripMargin
            )),
          arg[String]("<in>")
            .action((x, o) => o.updateCommand[Command.Convert](_.copy(in = x)))
            .text(wrap("Input file.")),
          arg[String]("<out>")
            .optional()
            .action((x, o) => o.updateCommand[Command.Convert](_.copy(out = Some(x))))
            .text(wrap("Output file.")),
        ),
      note(""),
      cmd("grade")
        .action((_, o) => o.copy(command = Some(Command.Grade())))
        .children(
          arg[String]("<in>")
            .action((x, o) => o.updateCommand[Command.Grade](_.copy(in = x)))
            .text(wrap("File to grade.")),
          arg[String]("<out>")
            .optional()
            .action((x, o) => o.updateCommand[Command.Grade](_.copy(out = Some(x))))
            .text(wrap("Output directory for answer files.")),
          opt[Unit]("export-answers")
            .action((_, o) => o.updateCommand[Command.Grade](_.copy(exportAnswers = true)))
            .text(wrap("Export answers to text files instead of grading.")),
          opt[Unit]("skip-on-parse-error")
            .action((_, o) => o.updateCommand[Command.Grade](_.copy(skipOnParseError = true)))
            .text(wrap("Skip grading on parse errors.")),
        ),

      // Webui commands
      note(""),
      cmd("codegen")
        .action((_, o) => o.copy(command = Some(Command.Codegen())))
        .text(wrapWide("Generate executable C code from a model file."))
        .children(
          arg[String]("<in>")
            .action((x, o) => o.updateCommand[Command.Codegen](_.copy(in = x)))
            .text(wrap("Input archive file, can be of the form file.kyx#entry.")),
          arg[String]("<out>")
            .optional()
            .action((x, o) => o.updateCommand[Command.Codegen](_.copy(out = Some(x))))
            .text(wrap("Output file (default: input file name with .c suffix).")),
          opt[Unit]("quantitative")
            .action((_, o) => o.updateCommand[Command.Codegen](_.copy(quantitative = true)))
            .text(wrap("Generate a quantitative instead of a boolean monitor.")),
          opt[Boolean]("interval")
            .action((x, o) => o.updateCommand[Command.Codegen](_.copy(interval = x)))
            .valueName("<bool>")
            .text(wrap(
              """true: Guard reals by interval arithmetic in floating point (recommended).
                |false: Skip interval arithmetic presuming no floating point errors.
                |""".stripMargin
            )),
          opt[Seq[String]]("vars")
            .action((x, o) => o.updateCommand[Command.Codegen](_.copy(vars = Some(x))))
            .valueName("<vars>")
            .text(wrap("Use ordered comma-separated list of variables, treating others as constant functions.")),
        ),
      note(""),
      cmd("convert-tactics")
        .action((_, o) => o.copy(command = Some(Command.ConvertTactics())))
        .text(wrapWide("Tactic conversions."))
        .children(
          arg[TacticConversion.Value]("<conversion>")
            .action((x, o) => o.updateCommand[Command.ConvertTactics](_.copy(conversion = x)))
            .text(wrap(
              s"""Conversion to perform.
                 |${possibleValues(TacticConversion.values)}
                 |""".stripMargin
            )),
          arg[String]("<in>")
            .action((x, o) => o.updateCommand[Command.ConvertTactics](_.copy(in = x)))
            .text(wrap("Input file.")),
          arg[String]("<out>")
            .action((x, o) => o.updateCommand[Command.ConvertTactics](_.copy(out = x)))
            .text(wrap("Output file.")),
        ),
      note(""),
      cmd("modelplex")
        .action((_, o) => o.copy(command = Some(Command.Modelplex())))
        .text(wrapWide("Synthesize a monitor from a model by proof with the ModelPlex tactic."))
        .children(
          arg[String]("<in>")
            .action((x, o) => o.updateCommand[Command.Modelplex](_.copy(in = x)))
            .text(wrap("Input file.")),
          arg[String]("<out>")
            .optional()
            .action((x, o) => o.updateCommand[Command.Modelplex](_.copy(out = Some(x))))
            .text(wrap("Output file.")),
          opt[String]("pt-out")
            .action((x, o) => o.updateCommand[Command.Modelplex](_.copy(ptOut = Some(x))))
            .valueName("<file>"),
          opt[Seq[String]]("vars")
            .action((x, o) => o.updateCommand[Command.Modelplex](_.copy(vars = Some(x))))
            .valueName("<vars>")
            .text(wrap("Use ordered comma-separated list of variables, treating others as constant functions.")),
          opt[Unit]("verify").action((_, o) => o.updateCommand[Command.Modelplex](_.copy(verify = true))),
          opt[Unit]("sandbox").action((_, o) => o.updateCommand[Command.Modelplex](_.copy(sandbox = true))),
          opt[ModelPlexKind.Value]("monitor")
            .action((x, o) => o.updateCommand[Command.Modelplex](_.copy(monitor = Some(x))))
            .valueName("<monitor>")
            .text(wrap(
              s"""What kind of monitor to generate with ModelPlex.
                 |${possibleValues(ModelPlexKind.values)}
                 |""".stripMargin
            )),
          opt[String]("fallback").action((x, o) => o.updateCommand[Command.Modelplex](_.copy(fallback = Some(x)))),
        ),
      note(""),
      cmd("repl")
        .action((_, o) => o.copy(command = Some(Command.Repl())))
        .text(wrapWide("Prove a model interactively from a command line REPL."))
        .children(
          arg[String]("<model>").action((x, o) => o.updateCommand[Command.Repl](_.copy(model = x))),
          arg[String]("<tactic>").optional().action((x, o) => o.updateCommand[Command.Repl](_.copy(tactic = Some(x)))),
          arg[String]("<scaladefs>")
            .optional()
            .action((x, o) => o.updateCommand[Command.Repl](_.copy(scaladefs = Some(x)))),
        ),
      note(""),
      cmd("ui").action((_, o) => o.copy(command = Some(Command.Ui))).text(wrapWide("Start web user interface.")),
    )
  }

  def parseArgs(name: String, args: Seq[String]): Options = {
    val parser = this.parser(name)

    // When parse() returns None, it failed to parse the arguments
    // and will have already printed some sort of error message.
    val options = OParser.parse(parser, args, Options(name = name, args = args)).getOrElse(sys.exit(1))

    if (options.help) {
      println(OParser.usage(parser))
      sys.exit()
    }

    if (options.copyright) {
      println(FullCopyright)
      sys.exit()
    }

    if (options.license) {
      println(License)
      sys.exit()
    }

    if (options.thirdPartyLicenses) {
      println(ThirdPartyLicenses)
      sys.exit()
    }

    options
  }
}
