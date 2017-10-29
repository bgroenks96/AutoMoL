package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.model.rules.ModelRule
import edu.osu.cse.groenkeb.logic.model.rules.UniversalFalsification
import edu.osu.cse.groenkeb.logic.model.rules.OrFalsification
import edu.osu.cse.groenkeb.logic.model.rules.NegationFalsification
import edu.osu.cse.groenkeb.logic.model.rules.ConditionalVerification
import edu.osu.cse.groenkeb.logic.model.rules.ExistentialVerification
import edu.osu.cse.groenkeb.logic.model.rules.ExistentialFalsification
import edu.osu.cse.groenkeb.logic.model.rules.NegationVerification
import edu.osu.cse.groenkeb.logic.model.rules.OrVerification
import edu.osu.cse.groenkeb.logic.model.rules.AndFalsification
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic.model.rules.AndVerification
import edu.osu.cse.groenkeb.logic.model.rules.ConditionalFalsification
import edu.osu.cse.groenkeb.logic.model.rules.UniversalVerification

object implicits {
  implicit def standardRules(implicit model: FirstOrderModel): RuleSet =
    RuleSet(Seq(ModelRule(model),
      NegationVerification(), NegationFalsification(),
      AndVerification(), AndFalsification(),
      OrVerification(), OrFalsification(),
      ConditionalVerification(), ConditionalFalsification(),
      UniversalVerification(model.domain), UniversalFalsification(model.domain),
      ExistentialVerification(model.domain), ExistentialFalsification(model.domain)))
}