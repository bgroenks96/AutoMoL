package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic.proof.Action
import edu.osu.cse.groenkeb.logic.proof.engine.learn.q.QModel.orderByHighestQValue

import scala.util.Random

trait QPolicy {
  def actionSequence(qvals: Seq[QValue]): Seq[Action]
}

final class EpsilonGreedy(var epsilon: Double, decay: Double = 0f, rand: Random = new Random()) extends QPolicy {
  def actionSequence(qvals: Seq[QValue]): Seq[Action] = {
    require(epsilon >= 0.0, "")
    val currentEpsilon = this.epsilon
    this.epsilon -= this.decay
    rand.nextFloat() match {
      case p if p > currentEpsilon => qvals.map(qv => qv.args.action)
      case _ => rand.shuffle(qvals.map(qv => qv.args.action))
    }
  }
}
