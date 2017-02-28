package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.AtomicSentence
import edu.osu.cse.groenkeb.logic.UnarySentence
import edu.osu.cse.groenkeb.logic.Predicate
import edu.osu.cse.groenkeb.logic.BinarySentence

case class FirstOrderModel(val domain: Domain, diagram: AtomicDiagram) extends Model {
  def evaluate(sentence: ModelSentence): Boolean = diagram.verify(sentence.toRelation.asInstanceOf[AtomicRelation])
}