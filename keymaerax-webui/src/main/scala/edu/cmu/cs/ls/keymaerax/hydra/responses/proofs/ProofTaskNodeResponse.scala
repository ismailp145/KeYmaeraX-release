/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package edu.cmu.cs.ls.keymaerax.hydra.responses.proofs

import edu.cmu.cs.ls.keymaerax.hydra.Helpers.nodeJson
import edu.cmu.cs.ls.keymaerax.hydra.{ProofTreeNode, Response}
import spray.json.JsValue

class ProofTaskNodeResponse(node: ProofTreeNode, marginLeft: Int, marginRight: Int) extends Response {
  def getJson: JsValue = nodeJson(node, withSequent = true, marginLeft, marginRight)._2
}
