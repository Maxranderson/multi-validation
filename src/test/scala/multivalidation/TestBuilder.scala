package multivalidation

import multivalidation.parsers.Parser

import scala.util.Try

object TestBuilder extends ValidationBuilder[EntryData] {
  implicit val entryParser: Parser[EntryData, EntryData] = new Parser[EntryData, EntryData] {
    override def parse(a: EntryData): Try[EntryData] = Try(a)
  }
}
