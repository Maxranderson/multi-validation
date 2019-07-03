package multivalidation

import org.scalatest.WordSpec
import TestBuilder._
import multivalidation.parsers.Parser

import scala.util.{Success, Try}

class StepSpec extends WordSpec {

  "A Step" when {
    "combined with a step" should {
      "return last invalidated" in {
        val invalidStringSize = Invalid("String has invalid size")
        val entryData = EntryData("")
        val step1 = Rule.verify[EntryData](_ => Valid).toStep
        val step2 = Rule.verify[EntryData](e => if(e.str.length < 2) invalidStringSize else Valid).toStep
        assert((step1 combine step2).run(entryData) == Success(entryData, Seq(invalidStringSize), true))
      }
    }
    "return a invalid and combined with another step" should {
      "not run next step" in {
        val invalidStringEmpty = Invalid("String is empty")
        val invalidStringSize = Invalid("String has invalid size")
        val entryData = EntryData("")
        val step1 = Rule.verify[EntryData](e => if(e.str.isEmpty) invalidStringEmpty else Valid).toStep
        val step2 = Rule.verify[EntryData](e => if(e.str.length < 2) invalidStringSize else Valid).toStep
        assert((step1 combine step2).run(entryData) == Success(entryData, Seq(invalidStringEmpty), true))
      }
    }
    "return valid result and combined with another step" should {
      "return empty validated result" in {
        val entryData = EntryData("")
        val step1 = Rule.verify[EntryData](_ => Valid).toStep
        val step2 = Rule.verify[EntryData](_ => Valid).toStep
        assert((step1 combine step2).run(entryData) == Success(entryData, Seq.empty, false))
      }
    }
  }

  "A step can be a validation" in {
    case class FinalData(n: Int)
    val entryData = EntryData("1")
    implicit val finalParser: Parser[EntryData, FinalData] = new Parser[EntryData, FinalData] {
      def parse(a: EntryData): Try[FinalData] = Try(FinalData(a.str.toInt))
    }
    val step1 = Rule.verify[EntryData](_ => Valid).toStep
    val step2 = Rule.verify[EntryData](_ => Valid).toStep
    val steps = step1 combine step2
    assert(steps.toValidation[FinalData].run(entryData).contains(Success(SuccessResult(FinalData(1)))))
  }

}
