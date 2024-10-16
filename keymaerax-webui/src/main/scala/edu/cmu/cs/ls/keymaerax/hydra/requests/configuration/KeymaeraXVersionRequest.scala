/*
 * Copyright (c) Carnegie Mellon University, Karlsruhe Institute of Technology.
 * See LICENSE.txt for the conditions of this license.
 */

package edu.cmu.cs.ls.keymaerax.hydra.requests.configuration

import edu.cmu.cs.ls.keymaerax.UpdateChecker
import edu.cmu.cs.ls.keymaerax.hydra.responses.configuration.KeymaeraXVersionResponse
import edu.cmu.cs.ls.keymaerax.hydra.{ReadRequest, Request, Response}
import edu.cmu.cs.ls.keymaerax.info.Version

class KeymaeraXVersionRequest extends Request with ReadRequest {
  override def resultingResponse(): Response = {
    new KeymaeraXVersionResponse(Version.toString, UpdateChecker.upToDate, UpdateChecker.latestVersion.map(_.toString))
  }
}
