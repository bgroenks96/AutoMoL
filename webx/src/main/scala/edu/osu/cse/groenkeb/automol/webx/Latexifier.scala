package edu.osu.cse.groenkeb.automol.webx

import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.model.rules._
import edu.osu.cse.groenkeb.logic._

object Latexifier {
  
  def latexPrint(proof: Proof): String = {
    var itr = ProofTraverser.preOrderTraversal(proof).iterator
    latexPrint(itr, "\\[") ++ "\\]"
  }
  
  private def latexPrint(itr: Iterator[Proof], proofString: String): String = {
    if (!itr.hasNext) proofString
    else itr.next() match {
      case CompleteProof(conc, prems) => conc.rule match {
        case NullRule() => ""
        case IdentityRule() =>
          proofString.concat(String.format("\\inferbasic[%s]{%s} ", 
                                           ruleToString(conc.rule), 
                                           sentenceToString(conc.sentence))).concat(latexPrint(itr, ""))
        case r@ModelRule(_) if conc.sentence != Absurdity =>
          proofString.concat(String.format("\\inferbasic[%s]{%s} ", 
                                           ruleToString(conc.rule), 
                                           sentenceToString(conc.sentence))).concat(latexPrint(itr, ""))
        case rule =>  proofString.concat(String.format("\\infer[%s]{%s}{%s} ",     
                                         ruleToString(rule), 
                                         sentenceToString(conc.sentence),
        		                             (conc.args.prems map {p => latexPrint(itr, "")}).fold("")((x, y) => x ++ y)))
      }
      case NullProof => proofString
    }
  }
  
  private def ruleToString(rule: Rule): String = {
    rule match {
      case AndVerification() => "\\wedge V"
      case AndFalsification()  => "\\wedge F"
      case OrVerification() => "\\vee V"
      case OrFalsification() => "\\vee F"
      case NegationVerification() => "\\not V"
      case NegationFalsification() => "\\not F"
      case UniversalVerification(_) => "\\forall V"
      case UniversalFalsification(_) => "\\forall F"
      case ExistentialVerification(_) => "\\exists V"
      case ExistentialFalsification(_) => "\\exists F"
      case ModelRule(_) => " M"
      case IdentityRule()        => ""
      case NullRule()            => ""
      //others go here...      
    }
  }
  
  private def sentenceToString(sentence: Sentence): String = {
    
    sentence match {
      case Absurdity                 => "\\bot"
      case AtomicSentence(x)         => x.toString()
      case UnarySentence(x, y)       => unaryConnectiveToString(y) ++ sentenceToString(x) 
      case BinarySentence(x, y, z)   => sentenceToString(x) ++ binaryConnectiveToString(z) ++ sentenceToString(y)
      case QuantifiedSentence(x, y)  => quantifiedSentenceToString(y) ++ sentenceToString(x) 
      case NullSentence              => ""
    }
  }
  
  private def unaryConnectiveToString(conn: UnaryConnective): String = {
    conn match{
      case Not() => "\\neg "
      case _     => "un:Error"
    }
  }
  
  private def binaryConnectiveToString(conn: BinaryConnective): String = {
    conn match{
      case And()     => "\\wedge "
      case Or()      => "\\vee "
      case Implies() => "\\rightarrow "
      case _         => "bin:Error"
    }
  }
  
  private def quantifiedSentenceToString(quant: Quantifier): String = {
    quant match{
      case ExistentialQuantifier(t) => "\\exists " + t.name.toString()
      case UniversalQuantifier(t)   => "\\forall " + t.name.toString()
      case _                        => "quant:error"
    }
  }
  
}
