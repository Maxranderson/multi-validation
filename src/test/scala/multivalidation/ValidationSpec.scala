package multivalidation

import org.scalatest.WordSpec
import TestBuilder._
import multivalidation.parsers.Parser

import scala.util.{Success, Try}

class ValidationSpec extends WordSpec {
  case class FinalData(n: Int)

  "A Validation case not match" should {
    "return None" in {
      val invalidStringEmpty = Invalid("String is empty")
      val entryData = EntryData("1")
      implicit val finalParser: Parser[EntryData, FinalData] = new Parser[EntryData, FinalData] {
        def parse(a: EntryData): Try[FinalData] = Try(FinalData(a.str.toInt))
      }
      val validation1 = Validation.ofCond[EntryData, FinalData](_.str.length > 3) {
        Rule.verify[EntryData](e => if(e.str.isEmpty) invalidStringEmpty else Valid).toStep
      }
      assert(validation1.run(entryData).isEmpty)
    }
  }

  "A Validation case match" should {
    "return Some" in {
      val invalidStringEmpty = Invalid("String is empty")
      val entryData = EntryData("1")
      implicit val finalParser: Parser[EntryData, FinalData] = new Parser[EntryData, FinalData] {
        def parse(a: EntryData): Try[FinalData] = Try(FinalData(a.str.toInt))
      }
      val validation1 = Validation.ofCond[EntryData, FinalData](_.str.length < 3) {
        Rule.verify[EntryData](e => if(e.str.isEmpty) invalidStringEmpty else Valid).toStep
      }
      assert(validation1.run(entryData).isDefined)
    }
  }

  "A Validation be partial" in {
    val invalidStringEmpty = Invalid("String is empty")
    val entryData = EntryData("1")
    implicit val finalParser: Parser[EntryData, FinalData] = new Parser[EntryData, FinalData] {
      def parse(a: EntryData): Try[FinalData] = Try(FinalData(a.str.toInt))
    }
    val validation1 = Validation.caseOf[EntryData, FinalData] {
      case EntryData(str) if str.length < 3 =>
        Rule.verify[EntryData](e => if(e.str.isEmpty) invalidStringEmpty else Valid).toStep
    }
    assert(validation1.run(entryData).isDefined)
  }

  "A Validation always apply" in {
    val invalidStringEmpty = Invalid("String is empty")
    val entryData = EntryData("1")
    implicit val finalParser: Parser[EntryData, FinalData] = new Parser[EntryData, FinalData] {
      def parse(a: EntryData): Try[FinalData] = Try(FinalData(a.str.toInt))
    }
    val validation1 = Validation.of[EntryData, FinalData] {
        Rule.verify[EntryData](e => if(e.str.isEmpty) invalidStringEmpty else Valid).toStep
    }
    assert(validation1.run(entryData).isDefined)
  }

  "A Validation" should {
    "be combined with another Validation" in {
      val invalidStringEmpty = Invalid("String is empty")
      val invalidStringSize = Invalid("String has invalid size")
      val entryData = EntryData("1")
      implicit val finalParser: Parser[EntryData, FinalData] = new Parser[EntryData, FinalData] {
        def parse(a: EntryData): Try[FinalData] = Try(FinalData(a.str.toInt))
      }
      val validation1 = Validation.ofCond[EntryData, FinalData](_.str.length > 3) {
          Rule.verify[EntryData](e => if(e.str.isEmpty) invalidStringEmpty else Valid).toStep
      }
      val validation2 = Validation.ofCond[EntryData, FinalData](_.str.length < 3) {
          Rule.verify[EntryData](e => if(e.str.length < 2) invalidStringSize else Valid).toStep
      }

      assert((validation1 or validation2).run(entryData).contains(Success(FailureResult(Seq(invalidStringSize)))))
    }
  }

}
