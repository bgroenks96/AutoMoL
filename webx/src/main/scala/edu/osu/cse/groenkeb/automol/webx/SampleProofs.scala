package edu.osu.cse.groenkeb.automol.webx

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.model.FirstOrderModel
import edu.osu.cse.groenkeb.logic.model.rules.AndVerification
import edu.osu.cse.groenkeb.logic.model.rules.ModelRule
import edu.osu.cse.groenkeb.logic.proof.Conclusion
import edu.osu.cse.groenkeb.logic.proof.Proof
import edu.osu.cse.groenkeb.logic.proof.ProudPremise
import edu.osu.cse.groenkeb.logic.proof.rules.BinaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.EmptyArgs

object SampleProofs {

  def Id = ProudPremise(atom("P")).proof

  def VerifyAtom_1 = {
    val model = FirstOrderModel.from(atom("P"))
    val mrule = ModelRule(model)
    val p = atom("P")
    Proof(Conclusion(p, mrule, EmptyArgs()), Set())
  }

  def VerifyAnd_1 = {
    val model = FirstOrderModel.from(atom("P"), atom("Q"))
    val andv = AndVerification()
    val mrule = ModelRule(model)
    val p = atom("P")
    val q = atom("Q")
    val pq = and(p, q)
    Proof(Conclusion(pq, andv, BinaryArgs(Proof(Conclusion(p, mrule, EmptyArgs()), Set()),
                                          Proof(Conclusion(q, mrule, EmptyArgs()), Set()))), Set())
  }

  def VerifyAnd_2 = {
    val model = FirstOrderModel.from(atom("P"), atom("Q"), atom("R"))
    val andv = AndVerification()
    val mrule = ModelRule(model)
    val p = atom("P")
    val q = atom("Q")
    val r = atom("R")
    val pq = and(p, q)
    val pqr = and(r, pq)
    //    Proof(Conclusion(pq, andv, BinaryArgs(Proof(Conclusion(p, mrule, EmptyArgs()), Set()),
    //                                          Proof(Conclusion(q, mrule, EmptyArgs()), Set()))), Set())

    Proof(Conclusion(pqr, andv, BinaryArgs(Proof(Conclusion(r, mrule, EmptyArgs()), Set()), Proof(Conclusion(pq, andv, BinaryArgs(Proof(Conclusion(p, mrule, EmptyArgs()), Set()),
                                           Proof(Conclusion(q, mrule, EmptyArgs()), Set()))), Set()))), Set())
  }

  private def atom(str: String) = Sentences.atom(str)

  private def and(left: Sentence, right: Sentence) = Sentences.and(left, right)
}