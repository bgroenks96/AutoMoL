package edu.osu.cse.groenkeb.automol.webx

import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.document
import scala.scalajs.js.annotation.JSExportTopLevel

object TestObject {
  
  def main(args: Array[String]): Unit = {
    
    var result = ProverStub.proveStub("P", "P")
    println(result)
    result = Latexifier.latexPrint(SampleProofs.VerifyAnd_1)
    println(result)
    updateText(document.getElementById("top"), result)
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
  
  //some buttons to make demoing easier
  @JSExportTopLevel("atomicButton_1")
  def atomicVerificationButton(): Unit = {
    updateText(document.getElementById("top"), Latexifier.latexPrint(SampleProofs.VerifyAtom_1))
  }
  
  @JSExportTopLevel("andButton_1")
  def andVerificationButton(): Unit = {
    updateText(document.getElementById("top"), Latexifier.latexPrint(SampleProofs.VerifyAnd_1))    
  }
  
  @JSExportTopLevel("andButton_2")
  def andVerificationButton1(): Unit = {
    updateText(document.getElementById("top"), Latexifier.latexPrint(SampleProofs.VerifyAnd_2))    
  }
  
  
  
  @JSExportTopLevel("search")
  def search(): Unit = {
    val input1 = document.getElementById("goal_id").asInstanceOf[html.Input]
    val goal = input1.value   
    val input2 = document.getElementById("prem_id").asInstanceOf[html.Input]
    val premises = input2.value
    val splitPrem = premises.split(',')

    val result = ProverStub.prove(goal, splitPrem:_*) 
    
    updateText(document.getElementById("top"), result)
  }
}