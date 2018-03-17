package edu.osu.cse.groenkeb.logic.parse.corepl

import scala.collection.mutable.Set

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.rules.core._
import edu.osu.cse.groenkeb.logic.utils.StatefulGenerator

final class CoreProofBuilder {
  val idGenerator = new StatefulGenerator[Int](0, i => i + 1)
  
  val undischargedAssumptions = scala.collection.mutable.Map[Sentence, Assumption]()
  
  def reset = {
    idGenerator.state = 0
  }
  
  def trivialProof(s: Sentence, undis: Seq[Sentence]): Proof = Assumption(s, Some(IntBinding(idGenerator.next))).proof
  
  def majorProof(s: Sentence, minorProofs: Proof*): Proof = trivialProof(s, minorProofs.flatMap { p => p.undischarged.map { a => a.sentence } })
  
  def proof(s: Sentence, rule: Rule, args: RuleArgs, undis: Seq[Sentence]): Proof = rule match {
    case NegationIntroduction => negationIntro(s, args, undis)
    case IfIntroduction => ifIntro(s, args, undis)
    case IfElimination => ifElim(s, args, undis)
    case AndElimination => andElim(s, args, undis)
    case OrElimination => orElim(s, args, undis)
    case _ => Proof(s, rule, args, assumptionsFrom(args))
  }
  
  private def negationIntro(s: Sentence, args: RuleArgs, undis: Seq[Sentence]): Proof = s match {
    case Not(a) if args.prems.length == 1 =>
      val discharged = resolveDischarges(NegationIntroduction, (a, args.prems.head))
      assert(!discharged.isEmpty, s"Could not find assumption $a for discharge of rule $NegationIntroduction")
      Proof(s, NegationIntroduction, args, assumptionsFrom(args) -- discharged, Some(bindGroup(discharged)))
    case _ => ???
  }
  
  private def ifIntro(s: Sentence, args: RuleArgs, undis: Seq[Sentence]): Proof = s match {
    case If(ante, _) if args.prems.length == 1 =>
      val discharged = resolveDischarges(IfIntroduction, (ante, args.prems.head))
      args.prems.head.conclusion match {
        // Require discharge for absurdity case
        case Absurdity =>
          assert(!discharged.isEmpty, s"Could not find assumption $ante for discharge of rule $IfIntroduction")
          Proof(s, IfIntroduction, args, assumptionsFrom(args) -- discharged, None)
        // Vacuous discharge for proof of consequent
        case _ if discharged.isEmpty =>
          Proof(s, IfIntroduction, args, assumptionsFrom(args) -- discharged, None)
        case _ =>
          Proof(s, IfIntroduction, args, assumptionsFrom(args) -- discharged, Some(bindGroup(discharged)))
      }
      
      
    case _ => ???
  }
  
  private def ifElim(s: Sentence, args: RuleArgs, undis: Seq[Sentence]): Proof = args match {
    case TernaryArgs(major, lminor, rminor) => major.sentence match {
      case If(_, cons) =>
        val discharged = resolveDischarges(IfElimination, (cons, rminor))
        assert(!discharged.isEmpty, s"Could not find assumption $cons for discharge of rule $IfElimination")
        Proof(s, IfElimination, args, assumptionsFrom(args) -- discharged, Some(bindGroup(discharged)))
      case _ => ???
    }
    case _ => ???
  }
  
  private def andElim(s: Sentence, args: RuleArgs, undis: Seq[Sentence]): Proof = args match {
    case BinaryArgs(major, minor) => major.sentence match {
      case And(left, right) =>
        val discharged = resolveDischarges(AndElimination, (left, minor), (right, minor))
        assert(!discharged.isEmpty, s"Could not find either assumption $left or $right for discharge of rule $AndElimination")
        Proof(s, AndElimination, args, assumptionsFrom(args) -- discharged, Some(bindGroup(discharged)))
      case _ => ???
    }
    case _ => ???
  }
  
  private def orElim(s: Sentence, args: RuleArgs, undis: Seq[Sentence]): Proof = args match {
    case TernaryArgs(major, lminor, rminor) => major.sentence match {
      case Or(left, right) =>
        val dischargedLeft = resolveDischarges(OrElimination, (left, lminor))
        val dischargedRight = resolveDischarges(OrElimination, (right, rminor))
        val hasLeft = dischargedLeft.exists { a => a.matches(left) }
        val hasRight = dischargedRight.exists { a => a.matches(right) }
        assert(hasLeft, s"Could not find assumption $left for discharge of rule $OrElimination")
        assert(hasRight, s"Could not find assumption $right for discharge of rule $OrElimination")
        val discharged = dischargedLeft ++ dischargedRight
        Proof(s, OrElimination, args, assumptionsFrom(args) -- discharged, Some(bindGroup(discharged)))
      case _ => ???
    }
    case _ => ???
  }
  
  private def resolveDischarges(rule: Rule, toDischarge: (Sentence, Proof)*): Seq[Assumption] = {
    for {
      (s, p) <- toDischarge
      a <- p.undischarged.filter { a => a.matches(s) }
    } yield {
      a
    }
  }
  
  private def bindGroup(assumptions: Seq[Assumption]): GroupBinding = GroupBinding(assumptions.flatMap { a => a.binding }:_*)
  
  private def assumptionsFrom(args: RuleArgs): scala.collection.immutable.Set[Assumption] = args.prems.flatMap { p => p.undischarged }.toSet
}
