package multivalidation

import org.scalatest.WordSpec
import multivalidation.parsers.Parser
import TestBuilder._

import scala.util.{Success, Try}

class RuleSpec extends WordSpec {

  "A Rule" when {
    "exception is thrown" should {
      "return Failure" in {
        assert(Rule.build[EntryData](_ => throw new Exception("Failure")).run(EntryData("dada")).isFailure)
      }
    }

    "is delayed" should {
      "not run" in {
        val t1 = System.currentTimeMillis()
        val delayed = Rule.delay {
          Thread.sleep(1000)
          "Dependency IO"
        }
        val t2 = System.currentTimeMillis()
        assert(t2 - t1 <= 500)
      }
    }

    "combined with a rule of same type" should {
      "return two invalid results" in {
        val invalidStringEmpty = Invalid("String is empty")
        val invalidStringSize = Invalid("String has invalid size")
        val entryData = EntryData("")
        val r1 = Rule.verify[EntryData](e => if(e.str.isEmpty) invalidStringEmpty else Valid)
        val r2 = Rule.verify[EntryData](e => if(e.str.length < 2) invalidStringSize else Valid)
        assert((r1 ++ r2).run(entryData) == Success(entryData, Seq(invalidStringEmpty, invalidStringSize)))
      }

      "return one almost result" in {
        val invalidStringSize = Almost("String has invalid size")
        val entryData = EntryData("")
        val r1 = Rule.verify[EntryData](_ => Valid)
        val r2 = Rule.verify[EntryData](e => if(e.str.length < 2) invalidStringSize else Valid)
        assert((r1 ++ r2).run(entryData) == Success(entryData, Seq(invalidStringSize)))
      }

      "return empty seq result" in {
        val entryData = EntryData("")
        val r1 = Rule.verify[EntryData](_ => Valid)
        val r2 = Rule.verify[EntryData](_ => Valid)
        assert((r1 ++ r2).run(entryData) == Success(entryData, Seq.empty) )
      }
    }

    "combined with a rule of another type" should {
      "run with parser and reducer and return one invalid" in {
        case class AnotherData(n: Int)
        def buildParser(anotherData: AnotherData): Parser[EntryData, (EntryData, AnotherData)] = new Parser[EntryData, (EntryData, AnotherData)] {
          override def parse(a: EntryData): Try[(EntryData, AnotherData)] = Try((a, anotherData))
        }
        val entryData = EntryData("")
        val finalData = (EntryData(""), AnotherData(1))
        val invalidNumber = Invalid("Invalid number")
        val r1 = Rule.verify[EntryData](_ => Valid)
        val r2 = Rule.verify[(EntryData, AnotherData)]{ case (_, another) => if(another.n < 2) invalidNumber else Valid}
        implicit val parser = buildParser(finalData._2)
        assert((r1 ++ r2).run(entryData) == Success(finalData, Seq(invalidNumber)))
      }
    }
  }

  "A Rule can be created from a delayed code" in {
    val invalidStringEmpty = Invalid("String is empty")
    val entryData = EntryData("")
    val r1 = for {
      _ <- Rule.delay {
        Thread.sleep(1000)
        "Dependency IO"
      }
      r <- Rule.verify[EntryData](e => if(e.str.isEmpty) invalidStringEmpty else Valid)
    } yield r

    val t1 = System.currentTimeMillis()
    r1.run(entryData)
    val t2 = System.currentTimeMillis()
    assert(t2 - t1 > 500)
  }

  "A Rule can be a step" in {
    val invalidStringEmpty = Invalid("String is empty")
    val invalidStringSize = Invalid("String has invalid size")
    val entryData = EntryData("")
    val r1 = Rule.verify[EntryData](e => if(e.str.isEmpty) invalidStringEmpty else Valid)
    val r2 = Rule.verify[EntryData](e => if(e.str.length < 2) invalidStringSize else Valid)
    assert((r1 ++ r2 toStep).run(entryData) == Success(None, Seq(invalidStringEmpty, invalidStringSize)))
  }

}
