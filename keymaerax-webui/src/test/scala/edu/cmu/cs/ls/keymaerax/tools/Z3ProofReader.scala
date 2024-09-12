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
    val term = parser.parseSExpr
    convert(term)
  }

  /** Sanitizes names by replacing `_`with [[USCORE]]. */
  private def sanitize(name: String): String = { name.replace("_", USCORE) }

  /** Converts a formula. */

  def convertProof(t: SExpr)(implicit defs: Map[String, Expression]): ProvableSig = t match {
    case Let(binding, bindings, term) => {
      // handle the Let case

      ???

    }
    case SForall(sortedVar, sortedVars, term) => {
      // Two for all cases, one is the smtlib forall and the other is the keymaerax forall. Used Alias to differentiate

      ???

      // handle the Forall case

    }

    case SList(SSymbol("proof") :: SList(SSymbol("let") :: bindings :: rest) :: _) => {
      // handle the proof case
      ???
    }
    case SList(SSymbol("and") :: SSymbol(p) :: SSymbol(q) :: Nil) => {
      // handle the and case
      ???
    }

    case SList(
          SSymbol("not") :: SList(SSymbol("or") :: SSymbol(p) :: SList(SSymbol("not") :: SSymbol(q) :: Nil) :: Nil) ::
          Nil
        ) => {

      // handle the not-or case
      ???
    }
    case SList(SList(SSymbol("proof") :: steps :: Nil) :: Nil) => convertProof(steps)

    case SList(SSymbol("let") :: SList(SList(name) :: expr) :: steps :: Nil) =>
      convertProof(steps)(defs + (name.toString -> DefaultSMTConverter(expr)))

    case SString(value) => {
      // handle the SString case
      ???
    }

    case _ => {
      // handle any other case
      throw new MatchError(t)
    }
  }

  // TODO
  // first one is checking to see if it starts with a proof
  // use the SMTConverters methods to convert the Z3 to a keymaerax formula

}
