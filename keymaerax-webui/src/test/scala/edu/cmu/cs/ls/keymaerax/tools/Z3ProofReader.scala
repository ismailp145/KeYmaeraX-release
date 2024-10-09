package edu.cmu.cs.ls.keymaerax.tools.ext

import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser.InterpretedSymbols
import edu.cmu.cs.ls.keymaerax.tools.ConversionException
import edu.cmu.cs.ls.keymaerax.tools.qe.DefaultSMTConverter
import smtlib.theories.{Core, Reals}
import smtlib.trees.Commands.{Logic, _}
import smtlib.trees.Terms.{
  Attribute,
  Forall => SForall,
  FunctionApplication,
  Identifier,
  QualifiedIdentifier,
  SKeyword,
  SSymbol,
  Term,
}
import smtlib.trees.Terms._

import java.io.{Reader, StringReader}
import scala.collection.mutable.ListBuffer

import edu.cmu.cs.ls.keymaerax.pt.ProvableSig
import edu.cmu.cs.ls.keymaerax.core.{Term => KeYmaeraTerm, _}
import edu.cmu.cs.ls.keymaerax.tools.qe.SMTConverter
import smtlib.trees.CommandsResponses.GetProofResponseSuccess

/** Reads [[Proofs]]s from SMT-LIB format: converts every (assert X) statement into an expression. */
object Z3ProofReader {
  val USCORE: String = "uscore"

  /** Reads a formula. */
  def readProof(s: String): ProvableSig = readExpr(s, convertProof(_)(Map.empty))
  // change to read proof
  // return provable sig

  /** Reads an expression using `convert` to turn it into the desired kind. */
  private def readExpr(s: String, convert: SExpr => ProvableSig): ProvableSig = {
    val r = new StringReader(s)
    val lexer = new smtlib.lexer.Lexer(r)
    val parser = new smtlib.parser.Parser(lexer)
    val term = parser.parseGetProofResponse
    convert(term)
  }

  /** Sanitizes names by replacing `_`with [[USCORE]]. */
  private def sanitize(name: String): String = { name.replace("_", USCORE) }

  /** Converts an SExpression. */

  def convertProof(t: SExpr)(implicit defs: Map[String, Expression]): ProvableSig = t match {

    case Let(VarBinding(name, term), bindings, remainder) => {
      println(bindings)
      var x = convertSExprToFormula(term)
      convertProof(remainder)(defs + (name.toString -> x))
      println(s"Matched let binding: $name with expression: $x")
      ???
    }
    case Let(binding, bindings, term) => {
      println(bindings)
      var x = convertSExprToFormula(term)
      convertProof(term)(defs + (binding.toString -> x))
      println(s"Matched let binding: $binding with expression: $x")
      ???
    }
    // Use `convertSExprToFormula` to convert the binding expression (conjunction in this case)
    // Match a `let` expression binding a variable ($x26) to a conjunction (and) of two predicates (_p_P and _p_Q)
    case SList(SSymbol("let") :: SList(SList(SSymbol(x) :: bindingExpr :: Nil) :: Nil) :: Nil) =>
      // Use `convertSExprToFormula` to convert the binding expression (conjunction in this case)
      val formula = convertSExprToFormula(bindingExpr)
      // convertProof(formula)(defs + (x -> formula))
      println(s"Matched let binding: $x with expression: $formula")
      ???

    case SList(SSymbol("let") :: SList(names :: x :: Nil) :: remainder) => {
      val formula = convertSExprToFormula(x)
      convertProof(names)(defs + (names.toString -> formula))
      println(s"Matched let binding: $names with expression: $formula")
      ???
    }
    case SList(
          SSymbol("let") :: SList(
            SList(SSymbol(x) :: SList(SSymbol(y) :: SSymbol("_p_P") :: SSymbol("_p_Q") :: Nil) :: Nil) :: Nil
          ) :: Nil
        ) => {
      println(s"Matched let binding: $x with expression: $y")
      ???
    }

    // May want to store the formula in another structure or handle it elsewhere

    case SList(
          SSymbol("let") ::
          SList(SSymbol("$x26") :: SList(SSymbol("and") :: SSymbol("_p_P") :: SSymbol("_p_Q") :: Nil) :: Nil) :: Nil
        ) =>
      println("Matched let binding")
      ???

    case SList(
          List(
            SSymbol("let"),
            SList(List(SList(List(SSymbol("$x26"), SList(List(SSymbol("and"), SSymbol("_p_P"), SSymbol("_p_Q"))))))),
          )
        ) => {
      print("Matched let binding")
      ???
    }

    // case SList(
    //       SSymbol("let") :: SList(
    //         SList(SSymbol("$x26") :: SList(SSymbol("and") :: SSymbol("_p_P") :: SSymbol("_p_Q") :: Nil) :: Nil) :: Nil
    //       )
    //     ) => {
    //   println("Matched let binding")
    //   ???
    // }

    case SList(SSymbol("let") :: SList(names :: x :: Nil) :: remainder) => {
      val formula = convertSExprToFormula(x)
      convertProof(names)(defs + (names.toString -> formula))
      println(s"Matched let binding: $names with expression: $formula")
      ???
    }
    case GetProofResponseSuccess(steps) => {
      convertProof(steps)
      ???
    }

    case _ => { throw new MatchError(t) }
  }

  /** had to create an sexpr to term method because Equal takes in terms */
  def convertSExprToFormula(sexpr: SExpr): Formula = sexpr match {

    case SSymbol(symbol) => PredOf(Function(symbol, None, Real, Bool), Nothing)
    case SList(SSymbol("not") :: arg :: Nil) => Not(convertSExprToFormula(arg))
    case SList(SSymbol("and") :: args) => args.map(convertSExprToFormula).reduceLeft(And.apply)
    case SList(SSymbol("or") :: args) => args.map(convertSExprToFormula).reduceLeft(Or.apply)
    case SList(SSymbol("=>") :: left :: right :: Nil) =>
      Imply(convertSExprToFormula(left), convertSExprToFormula(right))
    case SList(SSymbol("=") :: left :: right :: Nil) => Equal(convertSExprToTerm(left), convertSExprToTerm(right))
    case _ => throw new IllegalArgumentException(s"Unsupported SExpr: $sexpr")
    // had to create an sexpr to term method because Equal takes in terms
  }

  def convertSExprToTerm(sexpr: SExpr): KeYmaeraTerm = sexpr match {
    case SSymbol(symbol) => Variable(symbol)
    case SList(SSymbol("-") :: arg :: Nil) => Neg(convertSExprToTerm(arg))
    case SList(SSymbol("+") :: args) => args.map(convertSExprToTerm).reduceLeft(Plus.apply)
    case SList(SSymbol("*") :: args) => args.map(convertSExprToTerm).reduceLeft(Times.apply)
    case SList(SSymbol("/") :: left :: right :: Nil) => Divide(convertSExprToTerm(left), convertSExprToTerm(right))
    // handle other cases
  }

  // def convertSExprToProvableSig(sexpr: SExpr)(implicit defs: Map[String, Expression]): ProvableSig = {

  // convertSExprToFormula(sexpr Sexpr) match {
  //   case formula: Formula =>
  //     val sequent = Sequent(IndexedSeq(), IndexedSeq(formula))
  //     ProvableSig.startProof(sequent)
  //   case _ => throw new ConversionException("Invalid SMT-LIB expression")
  // }
  // }
  // def convertSExprToFormula(sexpr: SExpr)(implicit defs: Map[String, Expression]): ProvableSig =
  //   sexpr match { case SSymbol(symbol) => PredOf(Function(symbol, None, Real, Bool), Nothing) }

}
