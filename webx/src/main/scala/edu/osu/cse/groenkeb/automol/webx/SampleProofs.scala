package edu.osu.cse.groenkeb.automol.webx

import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.model._
import edu.osu.cse.groenkeb.logic.model.rules._

object SampleProofs {
    
  def Id = ProudPremise(atom("P")).proof
  
  
  def VerifyAtom_1 = {    
    val model = FirstOrderModel.from(atom("P"))
    val mrule = ModelRule(model)
    val p = atom("P")
    CompleteProof(Conclusion(p, mrule, EmptyArgs()), Set())
  }
  
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
  
  def VerifyAnd_2 = {
    val model = FirstOrderModel.from(atom("P"), atom("Q"), atom("R"))
    val andv = AndVerification()
    val mrule = ModelRule(model)
    val p = atom("P")
    val q = atom("Q")
    val r = atom("R")
    val pq = and(p, q)
    val pqr = and(r, pq)
//    CompleteProof(Conclusion(pq, andv, BinaryArgs(CompleteProof(Conclusion(p, mrule, EmptyArgs()), Set()),
//                                                  CompleteProof(Conclusion(q, mrule, EmptyArgs()), Set()))), Set())
                                                  
    CompleteProof(Conclusion(pqr, andv, BinaryArgs(CompleteProof(Conclusion(r, mrule, EmptyArgs()), Set()), CompleteProof(Conclusion(pq, andv, BinaryArgs(CompleteProof(Conclusion(p, mrule, EmptyArgs()), Set()),
                                                  CompleteProof(Conclusion(q, mrule, EmptyArgs()), Set()))), Set()))), Set())
  }

  
  private def atom(str: String) = Sentences.atom(str)
  
  private def and(left: Sentence, right: Sentence) = Sentences.and(left, right)
}