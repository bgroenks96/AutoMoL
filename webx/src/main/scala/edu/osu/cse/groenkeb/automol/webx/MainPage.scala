package edu.osu.cse.groenkeb.automol.webx

import scala.scalajs.js.annotation.JSExportTopLevel
import scalajs.js
import org.scalajs.jquery._
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.document

import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.encoding.json._
import edu.osu.cse.groenkeb.logic.web.modelvf.VerificationProofRequest
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode

object MainPage {
  
  def main(args: Array[String]): Unit = {
    val result = Latexifier.latexPrint(SampleProofs.VerifyAnd_1)
    println(result)
    // updateText(document.getElementById("top"), result)
  }
  
  def appendPar(targetNode: dom.Node, text: String) = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }
  
  def updateText(targetNode: dom.Node, text: String) = {
    println("updateText: " + text)
    targetNode.textContent = text
    js.Dynamic.global.updateProof()
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

    val reqJson = VerificationProofRequest(goal, splitPrem.toSeq).asJson
    def onSuccess(data: js.Dynamic, textStatus: String, jqXHR: JQueryXHR) {
      // Stringify JSON string from AJAX response and decode it into Proof using Circe
      val jsonStr = js.JSON.stringify(data, null)
      println("received response JSON from server: " + jsonStr)
      decode[Proof](jsonStr) match {
        case Right(proof) => updateText(document.getElementById("top"), Latexifier.latexPrint(proof))
        case Left(failure) => println("decoding error: " + failure)
      }
    }
    
    jQuery.ajax(js.Dynamic.literal(`type` = "POST",
                                   url = "prover/query",
                                   contentType = "application/json",
                                   data = reqJson.noSpaces,
                                   dataType = "json",
                                   success = onSuccess _
                                   ).asInstanceOf[JQueryAjaxSettings])
  }
}