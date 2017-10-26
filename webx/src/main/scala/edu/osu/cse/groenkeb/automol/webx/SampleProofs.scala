package edu.osu.cse.groenkeb.automol.webx

import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.model._
import edu.osu.cse.groenkeb.logic.model.rules._

object SampleProofs {
  def Id = ProudPremise(atom("P")).proof
  
  def VerifyAnd_1 = {
    val model = FirstOrderModel.from(atom("P"), atom("Q"))
    val andv = AndVerification()
    val mrule = ModelRule(model)
    val p = atom("P")
    val q = atom("Q")
    val pq = and(p, q)
    CompleteProof(Conclusion(pq, andv, BinaryArgs(CompleteProof(Conclusion(p, mrule, EmptyArgs()), Set()),
                                                  CompleteProof(Conclusion(q, mrule, EmptyArgs()), Set()))), Set())
  }
  
  private def atom(str: String) = Sentences.atom(str)
  
  private def and(left: Sentence, right: Sentence) = Sentences.and(left, right)
}