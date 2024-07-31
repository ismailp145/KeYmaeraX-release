file://<WORKSPACE>/keymaerax-webui/src/test/scala/edu/cmu/cs/ls/keymaerax/tools/SMTProofsTest.scala
### file%3A%2F%2F%2FUsers%2Fismail%2FProjects%2FKeYmaeraX-release%2Fkeymaerax-webui%2Fsrc%2Ftest%2Fscala%2Fedu%2Fcmu%2Fcs%2Fls%2Fkeymaerax%2Ftools%2FSMTProofsTest.scala:62: error: empty quoted identifier
  ```scala
  ^

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 2.13.13
Classpath:
<HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.13/scala-library-2.13.13.jar [exists ]
Options:



action parameters:
uri: file://<WORKSPACE>/keymaerax-webui/src/test/scala/edu/cmu/cs/ls/keymaerax/tools/SMTProofsTest.scala
text:
```scala
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
    round trip "x>=0".asFormula
    round trip "x+1>=0".asFormula
    round trip "\\exists x x>=0".asFormula
    round trip "\\forall x (x<=0|x>=0)".asFormula
  }
  ```scala
// prove: |- P() -> P() \/ Q()
import edu.cmu.cs.ls.keymaerax.core.*
val p = PredOf(Function("P", None, Unit, Bool), Nothing)
val q = PredOf(Function("Q", None, Unit, Bool), Nothing)
 
//    |- P -> P \/ Q
// --------------------
//    |- P -> P \/ Q
var proof = ProvableSig.startPlainProof(new Sequent(List.empty, List(Implies(p, Or(p, q)))))
 
// step 1 result: ImplyRight
//  P |- P \/ Q
// --------------------
//    |- P -> P \/ Q
proof = proof(ImplyRight(SuccPos(0)), 0)
 
// step 2 result: OrRight
//  P |- P, Q
// --------------------
//    |- P -> P \/ Q
proof = proof(OrRight(SuccPos(0)), 0)
 
// step 3 result: Id
//    *
// --------------------
//    |- P -> P \/ Q
proof = proof(Close(AntePos(0), SuccPos(0)), 0)
}

```



#### Error stacktrace:

```
scala.meta.internal.tokenizers.Reporter.syntaxError(Reporter.scala:23)
	scala.meta.internal.tokenizers.Reporter.syntaxError$(Reporter.scala:23)
	scala.meta.internal.tokenizers.Reporter$$anon$1.syntaxError(Reporter.scala:32)
	scala.meta.internal.tokenizers.Reporter.syntaxError(Reporter.scala:25)
	scala.meta.internal.tokenizers.Reporter.syntaxError$(Reporter.scala:25)
	scala.meta.internal.tokenizers.Reporter$$anon$1.syntaxError(Reporter.scala:32)
	scala.meta.internal.tokenizers.LegacyScanner.getBackquotedIdent(LegacyScanner.scala:454)
	scala.meta.internal.tokenizers.LegacyScanner.fetchToken(LegacyScanner.scala:327)
	scala.meta.internal.tokenizers.LegacyScanner.nextToken(LegacyScanner.scala:201)
	scala.meta.internal.tokenizers.LegacyScanner.foreach(LegacyScanner.scala:912)
	scala.meta.internal.tokenizers.ScalametaTokenizer.uncachedTokenize(ScalametaTokenizer.scala:23)
	scala.meta.internal.tokenizers.ScalametaTokenizer.$anonfun$tokenize$1(ScalametaTokenizer.scala:17)
	scala.collection.concurrent.TrieMap.getOrElseUpdate(TrieMap.scala:960)
	scala.meta.internal.tokenizers.ScalametaTokenizer.tokenize(ScalametaTokenizer.scala:17)
	scala.meta.internal.tokenizers.ScalametaTokenizer$$anon$2.apply(ScalametaTokenizer.scala:322)
	scala.meta.tokenizers.Api$XtensionTokenizeDialectInput.tokenize(Api.scala:22)
	scala.meta.tokenizers.Api$XtensionTokenizeInputLike.tokenize(Api.scala:13)
	scala.meta.internal.parsers.ScannerTokens$.apply(ScannerTokens.scala:917)
	scala.meta.internal.parsers.ScalametaParser.<init>(ScalametaParser.scala:34)
	scala.meta.parsers.Parse$$anon$1.apply(Parse.scala:36)
	scala.meta.parsers.Api$XtensionParseDialectInput.parse(Api.scala:22)
	scala.meta.internal.semanticdb.scalac.ParseOps$XtensionCompilationUnitSource.toSource(ParseOps.scala:15)
	scala.meta.internal.semanticdb.scalac.TextDocumentOps$XtensionCompilationUnitDocument.toTextDocument(TextDocumentOps.scala:179)
	scala.meta.internal.pc.SemanticdbTextDocumentProvider.textDocument(SemanticdbTextDocumentProvider.scala:54)
	scala.meta.internal.pc.ScalaPresentationCompiler.$anonfun$semanticdbTextDocument$1(ScalaPresentationCompiler.scala:469)
```
#### Short summary: 

file%3A%2F%2F%2FUsers%2Fismail%2FProjects%2FKeYmaeraX-release%2Fkeymaerax-webui%2Fsrc%2Ftest%2Fscala%2Fedu%2Fcmu%2Fcs%2Fls%2Fkeymaerax%2Ftools%2FSMTProofsTest.scala:62: error: empty quoted identifier
  ```scala
  ^