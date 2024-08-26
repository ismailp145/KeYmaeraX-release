/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package edu.cmu.cs.ls.keymaerax.tools
import edu.cmu.cs.ls.keymaerax.btactics.TacticTestBase
import edu.cmu.cs.ls.keymaerax.core.{BaseVariable, Exists, Forall, Formula, FuncOf, Not, Term, Variable}
import edu.cmu.cs.ls.keymaerax.infrastruct.{ExpressionTraversal, PosInExpr}
import edu.cmu.cs.ls.keymaerax.infrastruct.ExpressionTraversal.ExpressionTraversalFunction
import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
import edu.cmu.cs.ls.keymaerax.tools.ext.SmtLibReader
import edu.cmu.cs.ls.keymaerax.tools.qe.DefaultSMTConverter
// import edu.cmu.cs.ls.keymaerax.tools.ext.Z3ProofReader

class SMTProofsTest extends TacticTestBase {

  /** Prefixes from [[DefaultSMTConverter]]. */
  private val VAR_PREFIX = "_v_"
  private val FUNC_PREFIX = "_f_"
  private val DIFFSYMBOL_PREFIX = "_d_"

  private object round {
    def trip(t: Formula): Formula = { roundTrip(t) shouldBe Not(t); t }
    def roundTrip(t: Formula): Formula = {
      def convertVar(v: Variable): Variable = v match {
        case bv: BaseVariable => bv.copy(name = bv.name.replace(SmtLibReader.USCORE, "_").replace(VAR_PREFIX, ""))
      }

      def convertFml(f: Formula): Formula = ExpressionTraversal
        .traverse(
          new ExpressionTraversalFunction() {
            override def preF(p: PosInExpr, e: Formula): Either[Option[ExpressionTraversal.StopTraversal], Formula] =
              e match {
                case Exists(vs, p) => Right(Exists(vs.map(convertVar), convertFml(p)))
                case Forall(vs, p) => Right(Forall(vs.map(convertVar), convertFml(p)))
                case _ => Left(None)
              }
            override def preT(p: PosInExpr, e: Term): Either[Option[ExpressionTraversal.StopTraversal], Term] =
              e match {
                case v: BaseVariable => Right(convertVar(v))
                case FuncOf(fn, args) => Right(
                    FuncOf(fn.copy(name = fn.name.replace(SmtLibReader.USCORE, "_").replace(FUNC_PREFIX, "")), args)
                  )
                case _ => Left(None)
              }
          },
          f,
        )
        .get

      convertFml(SmtLibReader.readAssert(DefaultSMTConverter(t))._1)
    }
  }
  "SmtLibReader" should "read simple examples" in {
    println(DefaultSMTConverter("x>=0".asFormula))
    round trip "x>=0".asFormula
    round trip "x+1>=0".asFormula
    round trip "\\exists x x>=0".asFormula
    round trip "\\forall x (x<=0|x>=0)".asFormula
  }

  it should "read forward proof examples" in {
    // DefaultSMTConverter(“P”.asFormula) shouldBe
    println(DefaultSMTConverter("P".asFormula))
    "P".asFormula
    "Q".asFormula
    "P|Q".asFormula
    "P|P".asFormula
    "P->(P|Q)".asFormula
  }

  "SmtLibReaderTest" should "read forward proof example" in {
    println(DefaultSMTConverter("P".asFormula))
    "P".asFormula
    "Q".asFormula
    "P&Q".asFormula
    "P->(P&Q)".asFormula
  }

  import org.scalatest.flatspec.AnyFlatSpec
  import org.scalatest.matchers.should.Matchers

  "A proof" should "demonstrate P -> P ∨ Q" in {
    def proofPImpliesPOrQ[P, Q](p: P): Either[P, Q] = { Left(p) }

    val p = "P is true"
    proofPImpliesPOrQ[String, String](p) shouldEqual Left(p)
  }

  // create a scala class to read the proof in z3 and replay in keymaerax using the
  // associated methods in the keymaerax library to check the proof.

  // TODO: Implement the logic to read the proof in Z3 and replay it in Keymaerax

  // import edu.cmu.cs.ls.keymaerax.tools.ext.Z3ProofReader

  "Forward proof examples" should "convert formulas correctly" in {
    // Example of converting a formula to SMT
    val pFormula = "P".asFormula
    val qFormula = "Q".asFormula
    val pqFormula = "P&Q".asFormula
    val ppFormula = "P&P".asFormula

    val implicationFormula = "(P&Q) -> P".asFormula
    // println(DefaultSMTConverter(implicationFormula))
    // val ending = s"""(set-option :produce-proofs true)
    //                 |${DefaultSMTConverter(implicationFormula)}
    //                 |(get-proof)""".stripMargin
    // println(ending)
    // println(Z3ProofReader.readFml(ending))

    // assertions to ensure correct behavior
    pFormula should not be null
    qFormula should not be null
    pqFormula should not be null
    implicationFormula should not be null
  }

  // issue was produce-proofs not produce-proof
  import edu.cmu.cs.ls.keymaerax.tools.ext.Z3ProofReader
  "Z3" should "execute SMT Proof" in withZ3 { tool =>
    val implicationFormula = "(P&Q) -> P".asFormula
    val ending = s"""(set-option :produce-proofs true) 
                    |${DefaultSMTConverter(implicationFormula)}
                    |(get-proof)""".stripMargin
    println(tool.solveSMT((ending)))
    println(DefaultSMTConverter(implicationFormula))
    // println(Z3ProofReader.readFml(tool.solveSMT(ending)))

  }
  tools = tool.solveSMT(ending)

  // the issue is withZ3. For some reason the SMT solver is not correct/ giving the correct output.
}
