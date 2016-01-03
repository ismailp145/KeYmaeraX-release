/**
* Copyright (c) Carnegie Mellon University.
* See LICENSE.txt for the conditions of this license.
*/
package edu.cmu.cs.ls.keymaerax.parser

import testHelper.KeYmaeraXTestTags.{SlowTest, UsualTest, SummaryTest, CheckinTest}

import scala.collection.immutable._
import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser._
import org.scalatest.{PrivateMethodTester, Matchers, FlatSpec}

import test.RandomFormula

/**
 * Tests the parser on pretty prints of randomly generated formulas
 * @author Andre Platzer
 */
class RandomParserTests extends FlatSpec with Matchers {
  val randomTrials = 4000
  val randomComplexity = 6
  val rand = new RandomFormula()


  val pp = if (false) KeYmaeraXPrettyPrinter
  else new edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXWeightedPrettyPrinter
  val parser = KeYmaeraXParser

  def parseShouldBe(input: String, expr: Expression) = {
    val parse = parser(input)
    if (!(parse == expr)) {
      println("Reparsing" +
        "\nInput:      " + input +
        "\nParsed:     " + parse + " @ " + parse.getClass.getSimpleName +
        "\nExpression: " + KeYmaeraXPrettyPrinter.fullPrinter(parse))
      parse shouldBe expr
    }
  }

  "The parser" should "reparse pretty-prints of random formulas (checkin)" taggedAs(CheckinTest) in {test(10)}
  it should "reparse pretty-prints of random formulas (summary)" taggedAs(SummaryTest) in {test(50)}
  it should "reparse pretty-prints of random formulas (usual)" taggedAs(UsualTest) in {test(1000,10)}
  it should "reparse pretty-prints of random formulas (slow)" taggedAs(SlowTest) in {test(randomTrials,20)}

  private def test(randomTrials: Int= randomTrials, randomComplexity: Int = randomComplexity) =
    for (i <- 1 to randomTrials) {
      val e = rand.nextFormula(randomComplexity)
      val printed = pp.stringify(e)
      println("Random in: " + printed)
      val full = pp.fullPrinter(e)
      println("Fullform:  " + full)
      parseShouldBe(full, e)
      println("Reparsing: " + printed)
      parseShouldBe(printed, e)
    }

}
