/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package edu.cmu.cs.ls.keymaerax.parser

import edu.cmu.cs.ls.keymaerax.parser.StringConverter._
import edu.cmu.cs.ls.keymaerax.tags.CheckinTest
import edu.cmu.cs.ls.keymaerax.tools.KeYmaeraXTool
import edu.cmu.cs.ls.keymaerax.{Configuration, FileConfiguration}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, PrivateMethodTester}

/** @author Nathan Fulton */
@CheckinTest
class AxiomFileParserTest extends AnyFlatSpec with Matchers with PrivateMethodTester with BeforeAndAfterAll {

  private val loadAxiomString = PrivateMethod[String](Symbol("loadAxiomString"))

  override def beforeAll(): Unit = {
    Configuration.setConfiguration(FileConfiguration)
    KeYmaeraXTool.init(interpreter = KeYmaeraXTool.InterpreterChoice.LazySequential, initDerivationInfoRegistry = false)
  }

  "KeYmaeraXAxiomParser" should "parse the axiom file" in {
    // even AxiomBase is private[core], so get Class by Java reflection
    val clazz = Class.forName("edu.cmu.cs.ls.keymaerax.core.AxiomBase$")
    val axiomFile = clazz.getField("MODULE$").get(()) invokePrivate loadAxiomString()
    val axioms = KeYmaeraXAxiomParser(axiomFile)
    axioms.size shouldNot be <= 0
    // check for a sample
    axioms should contain("<> diamond", "![a;]!p(||) <-> <a;>p(||)".asFormula)
  }

  "DLAxiomParser" should "parse the axiom file" in {
    // even AxiomBase is private[core], so get Class by Java reflection
    val clazz = Class.forName("edu.cmu.cs.ls.keymaerax.core.AxiomBase$")
    val axiomFile = clazz.getField("MODULE$").get(()) invokePrivate loadAxiomString()
    val axioms = DLAxiomParser(axiomFile)
    axioms.size shouldNot be <= 0
    // check for a sample
    axioms should contain("<> diamond", "![a;]!p(||) <-> <a;>p(||)".asFormula)
  }

  "Both parsers" should "agree on the outcome" in {
    // even AxiomBase is private[core], so get Class by Java reflection
    val clazz = Class.forName("edu.cmu.cs.ls.keymaerax.core.AxiomBase$")
    val axiomFile = clazz.getField("MODULE$").get(()) invokePrivate loadAxiomString()
    val axioms1 = KeYmaeraXAxiomParser(axiomFile)
    val axioms2 = DLAxiomParser(axiomFile)
    axioms1 shouldBe axioms2
  }
}
