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
import javax.swing.plaf.synth.SynthListUI

/** Reads [[Proofs]]s from SMT-LIB format: converts every (assert X) statement into an expression. */
object Z3ProofReader {
  val USCORE: String = "uscore"

  /** Reads a formula. */
  def readProof(s: String): ProvableSig = readExpr(s, convertProof(_)(Map.empty, Map.empty))
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

  def convertProof(t: SExpr)(defs: Map[String, Expression], lemma: Map[String, ProvableSig]): ProvableSig = t match {

    case SList(SList(SSymbol("proof") :: steps :: Nil) :: Nil) => {
      println("Entered Proof")
      convertProof(steps)(defs, lemma)
    }

    case SList(SSymbol("let") :: SList(vars) :: rest :: Nil) => {
      print("entered Let: ")
      println(vars)
      convertProof(rest)(defs, lemma)

      // match on vars
      // seperate $ and @ names for lemma.

    }
    case SList(SSymbol(name) :: SList(rest) :: remainder) => {
      println(s"Entered Name, $name")
      ???
    }
     case SList(SSymbol(name) :: remainder) => {
      println(s"Entered SSymbol Name, $name")
      // supposedly this one may have worked
      ???
    }

    case GetProofResponseSuccess(steps) => { convertProof(steps)(defs, lemma) }

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

}
