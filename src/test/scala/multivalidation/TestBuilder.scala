package multivalidation

import multivalidation.parsers.Folder

import scala.util.Try

object TestBuilder extends ValidationBuilder[EntryData] {
  implicit val entryParser: Folder[EntryData, EntryData] = new Folder[EntryData, EntryData] {
    override def parse(a: EntryData): Try[EntryData] = Try(a)

    override def fold(a: EntryData, b: EntryData): Try[EntryData] = Try(a)
  }
}
