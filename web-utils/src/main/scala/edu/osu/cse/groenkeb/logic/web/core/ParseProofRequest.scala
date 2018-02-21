package edu.osu.cse.groenkeb.logic.web.core

final case class ParseProofRequest(parserType: ParserType, input: String)

sealed abstract class ParserType
final case object PrologParser extends ParserType
