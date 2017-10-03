package edu.osu.cse.groenkeb.logic.model.rules

import edu.osu.cse.groenkeb.logic.Absurdity
import edu.osu.cse.groenkeb.logic.And
import edu.osu.cse.groenkeb.logic.BinarySentence
import edu.osu.cse.groenkeb.logic.Implies
import edu.osu.cse.groenkeb.logic.Not
import edu.osu.cse.groenkeb.logic.Or
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.UnarySentence
import edu.osu.cse.groenkeb.logic.proof.rules.AbstractRule
import edu.osu.cse.groenkeb.logic.proof.rules.AnyProof
import edu.osu.cse.groenkeb.logic.proof.rules.BinaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.BinaryParams
import edu.osu.cse.groenkeb.logic.proof.rules.CompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.EmptyProof
import edu.osu.cse.groenkeb.logic.proof.rules.IncompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.NullResult
import edu.osu.cse.groenkeb.logic.proof.rules.RelevantProof
import edu.osu.cse.groenkeb.logic.proof.rules.Required
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.proof.rules.TernaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.TernaryParams
import edu.osu.cse.groenkeb.logic.proof.rules.UnaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.Variate
import edu.osu.cse.groenkeb.logic.proof.types.Assumption
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.Conclusion
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.utils.Empty
import edu.osu.cse.groenkeb.logic.Term
import edu.osu.cse.groenkeb.logic.QuantifiedSentence
import edu.osu.cse.groenkeb.logic.ExistentialQuantifier
import edu.osu.cse.groenkeb.logic.proof.rules.NParams
import edu.osu.cse.groenkeb.logic.proof.rules.OptionParams
import edu.osu.cse.groenkeb.logic.UniversalQuantifier
import edu.osu.cse.groenkeb.logic.model.Domain
import edu.osu.cse.groenkeb.logic.proof.rules.NArgs
import edu.osu.cse.groenkeb.logic.proof.rules.RuleParam
import edu.osu.cse.groenkeb.logic.proof.rules.UnaryParams
import edu.osu.cse.groenkeb.logic.proof.rules.Vacuous

abstract class FalsificationRule extends AbstractRule() {
  def yields(sentence: Sentence) = sentence match { case Absurdity => true; case _ => false }
}

case class NegationFalsification() extends FalsificationRule {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(UnarySentence(_, Not()),_,_), _) => true
    case _ => false
  }

  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity => args match {
      case BinaryArgs(CompleteProof(Conclusion(UnarySentence(operand, Not()), _, _), pa), minorProof) =>
        minorProof match {
          case CompleteProof(Conclusion(`operand`, _, _), pb) => 
            val major = UnarySentence(operand, Not())
            CompleteResult(CompleteProof(Absurdity, this, args, pa ++ pb + Assumption(major)))
          case _ => NullResult()
        }
      case UnaryArgs(CompleteProof(Conclusion(UnarySentence(operand, Not()), _, _), pa)) =>
        val major = UnarySentence(operand, Not())
        IncompleteResult(BinaryParams(EmptyProof(major), RelevantProof(operand, Vacuous(), Assumption(major))))
      case _ => NullResult()
    }
    case _ => NullResult()
  }

  override def toString = "~F"
}

case class AndFalsification() extends FalsificationRule {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(BinarySentence(_, _, And()), _, _), _) => true
    case _ => false
  }

  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity => args match {
      case BinaryArgs(CompleteProof(Conclusion(BinarySentence(left, right, And()), _, _), Empty()),
                      CompleteProof(Conclusion(Absurdity, _, _), prems)) if prems exists { p => p.matches(left) || p.matches(right) } =>
                        val major = Assumption(BinarySentence(left, right, And()))
                        val discharges = Set(Assumption(left), Assumption(right))
                        CompleteResult(CompleteProof(Conclusion(Absurdity, this, args), prems -- discharges + major))
      case UnaryArgs(CompleteProof(Conclusion(BinarySentence(left, right, And()), _, _), Empty())) =>
        IncompleteResult(BinaryParams(EmptyProof(BinarySentence(left, right, And())),
                                      RelevantProof(Absurdity, Variate(Assumption(left), Assumption(right)), Assumption(BinarySentence(left, right, And())))))
      case _ => NullResult()
    }
    case _ => NullResult()
  }

  override def toString = "&F"
}

case class OrFalsification() extends FalsificationRule() {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(BinarySentence(x, y, Or()), _, _), Empty()) => true
    case _ => false
  }

  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity => args match {
      case TernaryArgs(CompleteProof(Conclusion(BinarySentence(left, right, Or()), _, _), Empty()), arg1, arg2) => (arg1, arg2) match {
        case (CompleteProof(Conclusion(Absurdity, _, _), pleft), CompleteProof(Conclusion(Absurdity, _, _), pright))
          if exists(left).in(pleft) && exists(right).in(pright) =>
            val major = Assumption(BinarySentence(left, right, Or()))
            val discharges = Set(Assumption(left), Assumption(right))
            CompleteResult(CompleteProof(Conclusion(Absurdity, this, args), pleft ++ pright -- discharges + major))
        case _ => NullResult()
      }
      case UnaryArgs(CompleteProof(Conclusion(BinarySentence(left, right, Or()), _, _), Empty())) =>
        IncompleteResult(TernaryParams(EmptyProof(BinarySentence(left, right, Or())),
                                       RelevantProof(Absurdity, Required(Assumption(left)), Assumption(BinarySentence(left, right, Or()))),
                                       RelevantProof(Absurdity, Required(Assumption(right)), Assumption(BinarySentence(left, right, Or())))))
      case _ => NullResult()
    }
    case _ => NullResult()
  }

  override def toString = "+F"
}

case class ConditionalFalsification() extends FalsificationRule() {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(BinarySentence(_,_, Implies()),_,_), _) => true
    case _ => false
  }

  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity => args match {
      case TernaryArgs(CompleteProof(Conclusion(BinarySentence(ante, conseq, Implies()), _, _), Empty()), arg1, arg2) => (arg1, arg2) match {
        case (CompleteProof(Conclusion(`ante`, _, _), aprems), CompleteProof(Conclusion(Absurdity, _, _), cprems))
          if exists(conseq).in(cprems) =>
            val major = Assumption(BinarySentence(ante, conseq, Implies()))
            val discharges = Set(Assumption(conseq))
            CompleteResult(CompleteProof(Conclusion(Absurdity, this, args), aprems ++ cprems -- discharges + major))
        case _ => NullResult()
      }
      case UnaryArgs(CompleteProof(Conclusion(BinarySentence(ante, conseq, Implies()), _, _), Empty())) =>
        IncompleteResult(TernaryParams(EmptyProof(BinarySentence(ante, conseq, Implies())),
                                       RelevantProof(ante, Vacuous(), Assumption(BinarySentence(ante, conseq, Implies()))),
                                       RelevantProof(Absurdity, Required(Assumption(conseq)), Assumption(BinarySentence(ante, conseq, Implies())))))
      case _ => NullResult()
    }
    case _ => NullResult()
  }

  override def toString = ">F"
}

case class UniversalFalsification(domain: Domain) extends FalsificationRule() {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(QuantifiedSentence(_, UniversalQuantifier(_)), _, _), _) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity => args match {
      case BinaryArgs(CompleteProof(Conclusion(QuantifiedSentence(sentence, UniversalQuantifier(term)), _, _), Empty()), arg1) =>
        // Validate proof and extract relevant assumption for this falsification from the set of premises, if available.
        // If no match is found, the proof is not valid.
        validate(arg1, sentence, term) match {
          case Some(falsified) =>
            val major = Assumption(QuantifiedSentence(sentence, UniversalQuantifier(term)))
            val discharge = Assumption(falsified)
            CompleteResult(CompleteProof(Conclusion(Absurdity, this, args), arg1.premises - discharge + major))
          case None => NullResult()
        }
      case UnaryArgs(CompleteProof(Conclusion(QuantifiedSentence(sentence, UniversalQuantifier(term)), _, _), Empty())) =>
        IncompleteResult(OptionParams(this.domain.terms.toSeq.map {
          t => BinaryParams(EmptyProof(QuantifiedSentence(sentence, UniversalQuantifier(term))),
                            RelevantProof(Absurdity,
                                          Required(Assumption(sentence.substitute(term, t))),
                                          Assumption(QuantifiedSentence(sentence, UniversalQuantifier(term)))))
        }:_*))
    }
    case _ => NullResult()
  }
  
  // Like 'validate' for the other quantifier v/f rules except that it attempts to extract the relevant
  // falsified sentence from the premise set of the proof (there could be any number of valid options).
  private def validate(proof: Proof, sentence: Sentence, term: Term): Option[Sentence] = {
    def sub(t: Term) = sentence.substitute(term, t)
    proof.conclusion match {
      case Some(conc) if conc.sentence == Absurdity => this.domain.terms.collectFirst {
        case t if exists(sub(t)).in(proof.premises) => sub(t)
      }
      case _ => None
    }
  }
  
  override def toString = "UF"
}

case class ExistentialFalsification(domain: Domain) extends FalsificationRule() {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(QuantifiedSentence(_, ExistentialQuantifier(_)), _, _), _) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity => args match {
      case NArgs(Seq(CompleteProof(Conclusion(QuantifiedSentence(sentence, ExistentialQuantifier(term)),_,_), _),
                     proofs@_*)) =>
        val discharges = validate(proofs, sentence, term).map { s => Assumption(s) }
        discharges match {
          // Make sure all proofs have a corresponding discharge; otherwise fail.
          case d if d.length == proofs.length =>
            CompleteResult(CompleteProof(Conclusion(conc, this, args),
                                         proofs.flatMap { p => p.premises }.toSet -- discharges))
          case _ => NullResult()
        }
      case UnaryArgs(CompleteProof(Conclusion(QuantifiedSentence(sentence, ExistentialQuantifier(term)),_,_), _)) =>
        val disproofs = this.domain.terms.toSeq.map { 
          t => RelevantProof(Absurdity,
                             Required(Assumption(sentence.substitute(term, t))),
                             Assumption(QuantifiedSentence(sentence, ExistentialQuantifier(term))))
        }
        // Construct incomplete result with NParams, where first parameter is the standard proud premise
        // for falsification proofs, in this case our existential sentence. The disproofs are appended onto
        // the end to form the full sequence of required proofs.
        IncompleteResult(NParams(Seq(EmptyProof(QuantifiedSentence(sentence, ExistentialQuantifier(term))))
                                 ++ disproofs))
      case _ => NullResult()
    }
    case _ => NullResult()
  }
  
  private def validate(proofs: Seq[Proof], sentence: Sentence, term: Term): Seq[Sentence] = 
    this.domain.terms.toSeq.flatMap {
      t =>
        val sub = sentence.substitute(term, t)
        proofs.collect { case CompleteProof(Conclusion(Absurdity,_,_), premises) =>
          premises.collect { case p if p.matches(sub) => sub }
        }.flatten
  }
  
  override def toString = "EF"
}

