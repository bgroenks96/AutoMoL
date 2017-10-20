package proofdisplay

import edu.osu.cse.groenkeb.logic.proof._

object ProverStub {

  def proveStub(goal: String, premises: String*):String =  {
    var output: String = "\\[ \\infer[R]{" ++ goal ++ "}{"
    output = output ++ (premises.mkString(" & ")) ++ "}" ++ "\\]"
    output
  }


  //for now just assume that the input comes in formatted properly...
  def prove(goal: String, premises: String*): String = {
    ""
  }
}
