package proofdisplay

import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic._

object Latexifier {
  
  
    def latexPrint(proof: Proof): String = {
    var itr = ProofTraverser.preOrderTraversal(proof).iterator
    latexPrint(itr, "\\\\[") ++ "\\\\]"
  }
  
  //all this stuff probably deserves its own object 
  private def latexPrint(itr: Iterator[Proof], proofString: String):String = {
    if (!itr.hasNext)
      proofString;
    var proof = itr.next()
    if(proof.isInstanceOf[NullProof]){
      proofString
    }
    var conclusion = proof.conclusion.get
    if (conclusion.rule.isInstanceOf[NullRule]) 
      proofString;
    
    var newString = proofString.concat(String.format("\\\\infer[%s]{%s}{%s} ", 
                                          ruleToString(conclusion.rule), 
                                          sentenceToString(conclusion.sentence),
        		                              (conclusion.args.prems map {p => latexPrint(itr, "")}).fold("")((x, y) => x ++ y)))
    newString
  }
  
  private def ruleToString(rule: Rule): String = {
    rule match {
      case AndIntroductionRule() => "\\\\wedge -I"
      case AndEliminationRule()  => "\\\\wedge -E"
      case IdentityRule()        => "id"
      case NullRule()            => ""
      //others go here...      
    }
  }
  
  private def sentenceToString(sentence: Sentence): String = {
    
    sentence match {
      case Absurdity                 => "\\\\bot"
      case AtomicSentence(x)         => x.toString()
      case UnarySentence(x, y)       => unaryConnectiveToString(y) ++ sentenceToString(x) 
      case BinarySentence(x, y, z)   => sentenceToString(x) ++ binaryConnectiveToString(z) ++ sentenceToString(y)
      case QuantifiedSentence(x, y)  => quantifiedSentenceToString(y) ++ sentenceToString(x) 
      case NullSentence()            => ""
    }
  }
  
  private def unaryConnectiveToString(conn: UnaryConnective): String = {
    conn match{
      case Not() => "\\\\neg "
      case _     => "un:Error"
    }
  }
  
  private def binaryConnectiveToString(conn: BinaryConnective): String = {
    conn match{
      case And()     => "\\\\wedge "
      case Or()      => "\\\\vee "
      case Implies() => "\\\\rightarrow "
      case _         => "bin:Error"
    }
  }
  
  private def quantifiedSentenceToString(quant: Quantifier): String = {
    quant match{
      case ExistentialQuantifier(t) => "\\\\exists " + t.name.toString()
      case UniversalQuantifier(t)   => "\\\\forall " + t.name.toString()
      case _                        => "quant:error"
    }
  }
  
}
