package proofdisplay

import edu.osu.cse.groenkeb.logic.parse.DefaultPropOpMatcher
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.parse.SentenceParser
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.engine._;

object ProverStub {
  
  def proveStub(goal: String, premises: String*):String =  {
    var output: String = "\\[ \\infer[R]{" ++ goal ++ "}{"
    output = output ++ (premises.mkString(" & ")) ++ "}" ++ "\\]"
    output
  }
  
  
  //for now just assume that the input comes in formatted properly...
  def prove(goal: String, premises: String*): String = {
    implicit val matcher = new DefaultPropOpMatcher()
    implicit val proofStrategy = NaiveProofStrategy()
    val parser = new SentenceParser()
      
    val premisesSent = premises map {p => parser.parse(p)}
    val goalSent = parser.parse(goal)
    val rules = RuleSet(Seq(AndIntroductionRule(), AndEliminationRule(), NonContradictionRule())) 
    val proofContext = ProofContext(goalSent, rules, Premises.proud(premisesSent:_*))
    
    val propSolver = new ProofSolver()

    propSolver.prove(proofContext).filter { x => x.isInstanceOf[Success] }.head match {
     case Success(proof, context, _) => {
       Latexifier.latexPrint(proof)
     }
     //probably need a better way to signal failure, but this'll do for now
     case Failure(nullProof, _) => {
       return "Failure"
     }
   }
  }
}
