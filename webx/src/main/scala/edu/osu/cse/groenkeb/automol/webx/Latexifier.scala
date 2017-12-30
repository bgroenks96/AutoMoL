package edu.osu.cse.groenkeb.automol.webx

import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.model.rules._
import edu.osu.cse.groenkeb.logic._

object Latexifier {
  
  def latexPrint(proof: Proof): String = {
    var itr = ProofTraverser.preOrderTraversal(proof).iterator
    latexPrint(itr, "\\[")(new DischargeContext()) + "\\]"
  }
  
  private def latexPrint(itr: Iterator[Proof], proofString: String)(implicit context: DischargeContext): String = {
    if (!itr.hasNext) proofString
    else itr.next() match {
      case Proof(s, rule, args, prems, binding) => rule match {
        case NullRule => ""
        case IdentityRule if binding == None => proofString.concat(sentenceToString(s))
        case IdentityRule =>
          proofString.concat(String.format("\\inferbasic[%s]{%s} ", 
                                           ruleToString(rule, binding), 
                                           sentenceToString(s)))
        case r@ModelRule(_) if s != Absurdity =>
          proofString.concat(String.format("\\inferbasic[%s]{%s} ", 
                                           ruleToString(rule, binding), 
                                           sentenceToString(s)))
        case rule =>  proofString.concat(String.format("\\infer[%s]{%s}{%s} ",     
                                         ruleToString(rule, binding), 
                                         sentenceToString(s),
        		                             (args.prems map {p => latexPrint(itr, "")(context.copy)}).fold("")((x, y) => x + y)))
      }
    }
  }
  
  private def ruleToString(rule: Rule, binding: Option[Binding])(implicit context: DischargeContext): String =
    (binding match {
      case Some(IntBinding(id)) => "\\scriptsize{(%d)}  ".format(context.lookup(id))
      case _ => ""
    }) + "  \\small{%s}".format((rule match {
      case AndVerification => "\\wedge V"
      case AndFalsification  => "\\wedge F"
      case OrVerification => "\\vee V"
      case OrFalsification => "\\vee F"
      case NegationVerification => "\\neg V"
      case NegationFalsification => "\\neg F"
      case ConditionalVerification => "\\rightarrow V"
      case ConditionalFalsification => "\\rightarrow F"
      case UniversalVerification(_) => "\\forall V"
      case UniversalFalsification(_) => "\\forall F"
      case ExistentialVerification(_) => "\\exists V"
      case ExistentialFalsification(_) => "\\exists F"
      case ModelRule(_) => "M"
      case IdentityRule        => ""
      case NullRule            => ""
      case _ => "undef"
    }))
  
  private def sentenceToString(sentence: Sentence, parenthize: Boolean = false): String = {
    sentence match {
      case Absurdity                 => "\\bot"
      case AtomicSentence(x)         => x.toString()
      case UnarySentence(x, y)       => unaryConnectiveToString(y) + sentenceToString(x, true)
      case BinarySentence(x, y, z) if parenthize => "(%s%s%s)".format(sentenceToString(x, true), binaryConnectiveToString(z), sentenceToString(y, true))
      case BinarySentence(x, y, z) => "%s%s%s".format(sentenceToString(x, true), binaryConnectiveToString(z), sentenceToString(y, true))
      case QuantifiedSentence(x, y)  => quantifiedSentenceToString(y) + "[%s]".format(sentenceToString(x))
      case NullSentence              => ""
    }
  }
  
  private def unaryConnectiveToString(conn: UnaryConnective): String = {
    conn match{
      case Not => "\\neg "
      case _     => "un:Error"
    }
  }
  
  private def binaryConnectiveToString(conn: BinaryConnective): String = {
    conn match{
      case And     => "\\wedge "
      case Or      => "\\vee "
      case Implies => "\\rightarrow "
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
 
  type IdMap = scala.collection.mutable.Map[Int, Int]
  
  /**
   * Provides mutable context for transforming locally unique inference identifiers to globally unique values.
   * Inference bindings are assigned by the depth in the proof tree where the assumptions were introduced. This means
   * that some values may be duplicated across sub-trees. This class uses a StatefulGenerator to generate globally unique
   * values for each new inference binding. While traversing the proof, this context should be copied at each recursive step
   * to retain all context downward in the proof tree. The generator will be shared between all copy instances to
   * ensure that newly generated identifiers in each sub-tree are unique.
   */
  private class DischargeContext(private val generator: StatefulGenerator[Int],
                                 private val index: IdMap) {  
    def this() = this(new StatefulGenerator(1, i => i + 1), scala.collection.mutable.Map[Int, Int]())
    
    def this(parent: DischargeContext) = this(parent.generator, parent.index.clone())
    
    def lookup(id: Int) = index.getOrElse(id, {
      val next = generator.next
      index(id) = next
      next
    })
    
    /**
     * Creates a copy of this DischargeContext that has the same generator and existing indices.
     */
    def copy = new DischargeContext(this)
  }
}
