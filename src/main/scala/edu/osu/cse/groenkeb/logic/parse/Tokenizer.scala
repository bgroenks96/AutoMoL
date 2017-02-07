package edu.osu.cse.groenkeb.logic.parse

import scala.collection.LinearSeq

trait Tokenizer[T <: Token] {
  def tokenize(source: String): Seq[T]
}

trait Token {
  def value: String
}

