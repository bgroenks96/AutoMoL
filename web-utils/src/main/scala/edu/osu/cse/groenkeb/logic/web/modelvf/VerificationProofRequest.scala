package edu.osu.cse.groenkeb.logic.web.modelvf

import edu.osu.cse.groenkeb.logic.model.FirstOrderModel
import edu.osu.cse.groenkeb.logic.Sentence

import io.circe._

sealed case class VerificationProofRequest(query: String, modelDescription: Seq[String])

//object VerificationProofRequest {
//  implicit val verificationProofRequestEncoder = new Encoder[VerificationProofRequest] {
//    final def apply(req: VerificationProofRequest) = {
//      Json.obj(("query", Json.fromString(req.query)),
//               ("modelDesc", Json.arr(req.modelDescription.map { s => Json.fromString(s) }:_*)))
//    }
//  }
//  
//  implicit val verificationProofRequestDecoder: Decoder[VerificationProofRequest] = new Decoder[VerificationProofRequest] {
//    final def apply(c: HCursor): Decoder.Result[VerificationProofRequest] = {
//      c.downField("query").as[String] match {
//        case Right(query) => c.downField("modelDsc").as[List[String]] match {
//          case Right(strs) => Right(VerificationProofRequest(query, strs:_*))
//          case Left(failure) => Left(failure)
//        }
//        case Left(failure) => Left(failure)
//      }}
//  }
//}
