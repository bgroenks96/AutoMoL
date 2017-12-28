package edu.osu.cse.groenkeb.logic.model.rules

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.utils.Empty

abstract class FalsificationRule extends BaseRule {
  def yields(conclusion: Sentence) = conclusion is Absurdity
}

case object NegationFalsification extends FalsificationRule {
  def major(sentence: Sentence) = sentence match {
    case Not(_) => true
    case _ => false
  }
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = major match {
    case Some(Not(operand)) if (goal is Absurdity) =>
      Some(BinaryParams(EmptyProof(Not(operand)), RelevantProof(operand, Vacuous(), Assumption(Not(operand)))))
    case _ => None
  }
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = args match {
    case BinaryArgs(Proof(Not(operand), _, _, pmajor), minorProof) if (goal is Absurdity) => minorProof match {
      case Proof(`operand`,_,_, pminor) =>
        val major = Not(operand)
        Some(Proof(Absurdity, this, args, pmajor ++ pminor + Assumption(major)))
      case _ => None
    }
    case _ => None
  }

  override def toString = "~F"
}

case object AndFalsification extends FalsificationRule {
  def major(sentence: Sentence) = sentence match {
    case And(_,_) => true
    case _ => false
  }
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = major match {
    case Some(sentence @ And(left, right)) if (goal is Absurdity) =>
      Some(
        BinaryParams(
          EmptyProof(And(left, right)),
          RelevantProof(Absurdity, Variate(Assumption(left), Assumption(right)), Assumption(sentence))))
    case _ => None
  }
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = args match {
    case BinaryArgs(
      Proof(And(left, right), IdentityRule, _, _),
      Proof(Absurdity, _, _, prems)) if (goal is Absurdity) and (exists(left).in(prems) or exists(right).in(prems)) =>
      val major = Assumption(And(left, right))
      val discharges = Set(Assumption(left), Assumption(right))
      Some(Proof(Absurdity, this, args, prems -- discharges + major))
    case _ => None
  }

  override def toString = "&F"
}

case object OrFalsification extends FalsificationRule {
  def major(sentence: Sentence) = sentence match {
    case Or(_,_) => true
    case _ => false
  }
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = major match {
    case Some(sentence @ Or(left, right)) =>
      Some(TernaryParams(
        EmptyProof(Or(left, right)),
        RelevantProof(Absurdity, Required(Assumption(left)), Assumption(Or(left, right))),
        RelevantProof(Absurdity, Required(Assumption(right)), Assumption(Or(left, right)))))
    case _ => None
  }
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = args match {
    case TernaryArgs(
      Proof(Or(left, right), IdentityRule, _, _),
      Proof(Absurdity, _, _, pleft),
      Proof(Absurdity, _, _, pright)) if (goal is Absurdity) and exists(left).in(pleft) and exists(right).in(pright) =>
      val major = Assumption(Or(left, right))
      val discharges = Set(Assumption(left), Assumption(right))
      Some(Proof(Absurdity, this, args, pleft ++ pright -- discharges + major))
    case _ => None
  }

  override def toString = "+F"
}

case object ConditionalFalsification extends FalsificationRule {
  def major(sentence: Sentence) = sentence match {
    case Implies(_,_) => true
    case _ => false
  }
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = major match {
    case Some(sentence @ Implies(ante, conseq)) =>
      Some(TernaryParams(EmptyProof(Implies(ante, conseq)),
        RelevantProof(ante, Vacuous(), Assumption(Implies(ante, conseq))),
        RelevantProof(Absurdity, Required(Assumption(conseq)), Assumption(Implies(ante, conseq)))))
    case _ => None
  }
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = args match {
    case TernaryArgs(
      Proof(Implies(ante, conseq), IdentityRule,_,_),
      antecedentProof,
      Proof(Absurdity, _, _, cprems)) if (goal is Absurdity) and exists(conseq).in(cprems) =>
        antecedentProof match {
          case Proof(`ante`,_,_, aprems) =>
            val major = Assumption(Implies(ante, conseq))
            val discharges = Set(Assumption(conseq))
            Some(Proof(Absurdity, this, args, aprems ++ cprems -- discharges + major))
          case _ => None
        }
    case _ => None
  }

  override def toString = ">F"
}

case class UniversalFalsification(domain: Domain) extends FalsificationRule {
  def major(sentence: Sentence) = sentence match {
    case ForAll(_,_) => true
    case _ => false
  }
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = major match {
    case Some(ForAll(term, sentence)) if goal is Absurdity =>
      Some(OptionParams(domain.terms.toSeq.map {
        t =>
          BinaryParams(
            EmptyProof(
              ForAll(term, sentence)),
            RelevantProof(
              Absurdity,
              Required(Assumption(sentence.substitute(term, t))),
              Assumption(ForAll(term, sentence))))
      }: _*))
    case _ => None
  }
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = {
    // Like 'validate' for the other quantifier v/f rules except that it attempts to extract the relevant
    // falsified sentence from the premise set of the proof (there could be any number of valid options).
    def validate(proof: Proof, sentence: Sentence, term: Term): Option[Sentence] = {
      def sub(t: Term) = sentence.substitute(term, t)
      proof.conclusion match {
        case Absurdity => domain.terms.collectFirst {
          case t if exists(sub(t)).in(proof.undischarged) => sub(t)
        }
        case _ => None
      }
    }
    
    args match {
      case BinaryArgs(Proof(ForAll(term, sentence), IdentityRule,_,_), arg1) if goal is Absurdity =>
        // Validate proof and extract relevant assumption for this falsification from the set of premises, if available.
        // If no match is found, the proof is not valid.
        validate(arg1, sentence, term) match {
          case Some(falsified) =>
            val major = Assumption(ForAll(term ,sentence))
            val discharge = Assumption(falsified)
            Some(Proof(Absurdity, this, args, arg1.undischarged - discharge + major))
          case None => None
        }
      case _ => None
    }
  }
  
  override def toString = "UF"
}

case class ExistentialFalsification(domain: Domain) extends FalsificationRule {
  def major(sentence: Sentence) = sentence match {
    case Exists(_,_) => true
    case _ => false
  }
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = major match {
    case Some(Exists(term, sentence)) if goal is Absurdity =>
      val disproofs = domain.terms.toSeq.map {
        t =>
          RelevantProof(
            Absurdity,
            Required(Assumption(sentence.substitute(term, t))),
            Assumption(Exists(term, sentence)))
      }
      // Construct incomplete result with NParams, where first parameter is the standard proud premise
      // for falsification proofs, in this case our existential sentence. The disproofs are appended onto
      // the end to form the full sequence of required proofs.
      Some(NParams(Seq(EmptyProof(Exists(term, sentence))) ++ disproofs))
    case _ => None
  }
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = {
    def validate(proofs: Seq[Proof], sentence: Sentence, term: Term): Seq[Sentence] =
      domain.terms.toSeq.flatMap {
        t =>
          val sub = sentence.substitute(term, t)
          proofs.collect {
            case Proof(Absurdity,_,_, premises) =>
              premises.collect { case p if p.matches(sub) => sub }
          }.flatten
      }
    
    args match {
      case NArgs(Seq(Proof(Exists(term, sentence), IdentityRule,_,_), proofs @ _*)) if goal is Absurdity =>
        val discharges = validate(proofs, sentence, term).map { s => Assumption(s) }
        discharges match {
          // Make sure all proofs have a corresponding discharge; otherwise fail.
          case d if d.length == proofs.length =>
            Some(Proof(Absurdity, this, args, proofs.flatMap { p => p.undischarged }.toSet -- discharges))
          case _ => None
        }
      case _ => None
    }
  }
  
  override def toString = "EF"
}

