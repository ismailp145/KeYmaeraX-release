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
import edu.cmu.cs.ls.keymaerax.btactics.DerivationInfoRegistry.convert
import edu.cmu.cs.ls.keymaerax.tools.qe.DefaultSMTConverter._

/** Reads [[Proofs]]s from SMT-LIB format: converts every (assert X) statement into an expression. */
object Z3ProofReader {
  val USCORE: String = "uscore"

  /** Reads a formula. */
  def readProof(s: String): ProvableSig = readExpr(s, convertProof(_)(Map.empty, Map.empty))

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
  def convertProof(t: SExpr)(defs: Map[String, Formula], lemma: Map[String, ProvableSig]): ProvableSig = t match {

    case SList(SList(SSymbol("proof") :: steps :: Nil) :: Nil) => {
      println("Entered Proof")
      convertProof(steps)(defs, lemma)
    }

    case SList(SSymbol("let") :: SList(vars) :: rest :: Nil) => {
      println(s"Entered Let: $vars")

      vars.head match {

        case SList(SSymbol(x) :: y :: Nil) if (x.startsWith("$")) => {
          println(s"Extracted Variable: $x , $y")

          var nameMap = defs +
            (x ->
              convertSExprToFormula(
                y,
                defs,
              )) // want to convert the rest of the sexpr to a formula so that it can be mapped
          convertProof(rest)(nameMap, lemma)
        }

        case SList(SSymbol(x) :: y :: Nil) if (x.startsWith("@")) => {
          println(s"Extracted Lemma : $x")
          // println("y = \n" + y.mkString("\n"))
          println("y = " + y)

          var lemmaMap = lemma +
            (x -> convertProof(y)(defs, lemma)) // mapping string lemma to provable sig on vars.head
          println(lemmaMap)
          convertProof(rest)(defs, lemmaMap)

        }

        case _ =>
          println("No match found")
          convertProof(rest)(defs, lemma)
      }

    }
    // case SList(SSymbol("asserted") :: remainder) => { ??? }

    // case SList(SSymbol(name) :: remainder) => {
    //   println(s"Entered SSymbol Name 1: $name, $remainder")
    //   convertProof(remainder.head)(defs, lemma)
    // }

    // case SList(SSymbol(name) :: remainder :: Nil) => {
    //   println(s"Entered SSymbol Name 2: $name, $remainder")
    //   convertProof(remainder)(defs, lemma)
    // }

    case GetProofResponseSuccess(steps) => { convertProof(steps)(defs, lemma) }

    // case SSymbol(name) if name.startsWith("$") => {
    // TO DO NEED TO FIX
    // print(s"This is name: $name")
    //   nameMap(name) // so that it can return formula but need to convert to provable sig
    // ???
    // }

    // case SSymbol(name) if name.startsWith("@") => lemmaMap(name) // so that it can return provable sig
    case y => {
      println(y)
      throw new MatchError(t)
    }
  }

  /** Converts SExpression to Formula */
  def convertSExprToFormula(sexpr: SExpr, defs: Map[String, Formula]): Formula = sexpr match {

    case SSymbol(symbol) if symbol.startsWith("$") => defs(symbol)
    case SSymbol(symbol) => PredOf(nameFromIdentifier(symbol).asInstanceOf[Function], Nothing)

    case SList(SSymbol("not") :: arg :: Nil) => Not(convertSExprToFormula(arg, defs))
    case SList(SSymbol("and") :: args) => args.map(convertSExprToFormula(_, defs)).reduceLeft(And.apply)
    case SList(SSymbol("or") :: args) => args.map(convertSExprToFormula(_, defs)).reduceLeft(Or.apply)
    case SList(SSymbol("=>") :: left :: right :: Nil) =>
      Imply(convertSExprToFormula(left, defs), convertSExprToFormula(right, defs))
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
