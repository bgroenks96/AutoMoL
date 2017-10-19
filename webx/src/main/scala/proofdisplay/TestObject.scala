package main.scala.proofdisplay

import org.scalajs.dom
import org.scalajs.dom.html
import dom.document
import scala.scalajs.js.annotation.JSExportTopLevel
import proofdisplay.ProverStub

object TestObject {
  def main(args: Array[String]): Unit = {
//       appendPar(document.body, "$$\\mathcal{test}$$")
  }
  
  def appendPar(targetNode: dom.Node, text: String) = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }
  
  def updateText(targetNode: dom.Node, text: String) = {
    targetNode.textContent = text
  }
  
  @JSExportTopLevel("search")
  def search(): Unit = {
    val input1 = document.getElementById("goal_id").asInstanceOf[html.Input]
    val goal = input1.value   
    val input2 = document.getElementById("prem_id").asInstanceOf[html.Input]
    val premises = input2.value
    val splitPrem = premises.split(',')

    //pass input to proof facade for processing    
   
    //fastOptJS fails on this - 'linking error'
    val result = ProverStub.prove(goal, splitPrem:_*) 
    
    //but not this
    //val result = ProverStub.proveStub(goal, splitPrem:_*) 

    
    updateText(document.getElementById("top"), result)
  }
}