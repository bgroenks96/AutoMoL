package edu.osu.cse.groenkeb.logic.parse.corepl

import scala.collection.mutable.Set

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.rules.core._
import edu.osu.cse.groenkeb.logic.utils.StatefulGenerator

final class CoreProofBuilder {
  val idGenerator = new StatefulGenerator[Int](0, i => i + 1)
  
  def trivialProof(s: Sentence, undis: Seq[Sentence]): Proof = {
    val assumptions = Assumption(s, Some(IntBinding(idGenerator.next))) +: toAssumptions(undis).filterNot { a => a.matches(s) }
    val idProof = assumptions.head.proof
    Proof(s, idProof.rule, idProof.args, assumptions.toSet, idProof.binding)
  }
  
  def proof(s: Sentence, rule: Rule, args: RuleArgs, undis: Seq[Sentence]): Proof = rule match {
    case NegationIntroduction => negationIntro(s, args, undis)
    case IfIntroduction => ifIntro(s, args, undis)
    case IfElimination => ifElim(s, args, undis)
    case AndElimination => andElim(s, args, undis)
    case OrElimination => orElim(s, args, undis)
    case _ => Proof(s, rule, args, toAssumptions(undis).toSet)
  }
  
  private def negationIntro(s: Sentence, args: RuleArgs, undis: Seq[Sentence]): Proof = s match {
    case Not(a) =>
      val assumptions = resolve(NegationIntroduction, args, a)
      assert(!assumptions.isEmpty, s"Could not find assumption $a for discharge of rule $NegationIntroduction")
      Proof(s, NegationIntroduction, args, assumptions.toSet, Some(bindGroup(assumptions)))
    case _ => ???
  }
  
  private def ifIntro(s: Sentence, args: RuleArgs, undis: Seq[Sentence]): Proof = s match {
    case Implies(ante, _) =>
      val assumptions = resolve(IfIntroduction, args, ante)
      assert(!assumptions.isEmpty, s"Could not find assumption $ante for discharge of rule $IfIntroduction")
      Proof(s, IfIntroduction, args, assumptions.toSet, Some(bindGroup(assumptions)))
    case _ => ???
  }
  
  private def ifElim(s: Sentence, args: RuleArgs, undis: Seq[Sentence]): Proof = args.prems.headOption match {
    case Some(major) => major.sentence match {
      case Implies(_, cons) =>
        val assumptions = resolve(IfElimination, args, cons)
        assert(!assumptions.isEmpty, s"Could not find assumption $cons for discharge of rule $IfElimination")
        Proof(s, IfElimination, args, assumptions.toSet, Some(bindGroup(assumptions)))
      case _ => ???
    }
    case None => ???
  }
  
  private def andElim(s: Sentence, args: RuleArgs, undis: Seq[Sentence]): Proof = args.prems.headOption match {
    case Some(major) => major.sentence match {
      case And(left, right) =>
        val assumptions = resolve(AndElimination, args, left, right)
        assert(!assumptions.isEmpty, s"Could not find either assumption $left or $right for discharge of rule $AndElimination")
        Proof(s, AndElimination, args, assumptions.toSet, Some(bindGroup(assumptions)))
      case _ => ???
    }
    case None => ???
  }
  
  private def orElim(s: Sentence, args: RuleArgs, undis: Seq[Sentence]): Proof = args.prems.headOption match {
    case Some(major) => major.sentence match {
      case Or(left, right) =>
        val assumptions = resolve(OrElimination, args, left, right)
        val hasLeft = assumptions.exists { a => a.matches(left) }
        val hasRight = assumptions.exists { a => a.matches(right) }
        assert(hasLeft, s"Could not find assumption $left for discharge of rule $OrElimination")
        assert(hasRight, s"Could not find assumption $right for discharge of rule $OrElimination")
        Proof(s, OrElimination, args, assumptions.toSet, Some(bindGroup(assumptions)))
      case _ => ???
    }
    case None => ???
  }
  
  private def resolve(rule: Rule, args: RuleArgs, discharged: Sentence*): Seq[Assumption] = {
    for {
      s <- discharged
      a <- args.prems.flatMap { p => p.undischarged }.find { a => a.matches(s) }
    } yield a
  }
  
  private def bindGroup(assumptions: Seq[Assumption]): GroupBinding = GroupBinding(assumptions.flatMap { a => a.binding }:_*)
  
  private def toAssumptions(undis: Seq[Sentence]) = undis.map { a => Assumption(a) }
}
