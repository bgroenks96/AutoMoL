package edu.osu.cse.groenkeb.logic.web.modelvf

import edu.osu.cse.groenkeb.logic.model.FirstOrderModel
import edu.osu.cse.groenkeb.logic.Sentence

case class VerificationProofRequest(query: Sentence, model: FirstOrderModel)