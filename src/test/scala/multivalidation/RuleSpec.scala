package multivalidation

import org.scalatest.WordSpec
import multivalidation.parsers.Folder
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
        assert((r1 combine r2).run(entryData) == Success(entryData, Seq(invalidStringEmpty, invalidStringSize)))
      }

      "return one almost result" in {
        val invalidStringSize = Almost("String has invalid size")
        val entryData = EntryData("")
        val r1 = Rule.verify[EntryData](_ => Valid)
        val r2 = Rule.verify[EntryData](e => if(e.str.length < 2) invalidStringSize else Valid)
        assert((r1 combine r2).run(entryData) == Success(entryData, Seq(invalidStringSize)))
      }

      "return empty seq result" in {
        val entryData = EntryData("")
        val r1 = Rule.verify[EntryData](_ => Valid)
        val r2 = Rule.verify[EntryData](_ => Valid)
        assert((r1 combine r2).run(entryData) == Success(entryData, Seq.empty) )
      }
    }

    "combined with a rule of another type" should {
      "run with parser and reducer and return one invalid" in {
        case class AnotherData(n: Int)
        implicit val folder: Folder[EntryData, (EntryData, AnotherData)] = new Folder[EntryData, (EntryData, AnotherData)] {
          override def fold(a: EntryData, b: (EntryData, AnotherData)): Try[(EntryData, AnotherData)] = Try((a, b._2))

          override def parse(a: (EntryData, AnotherData)): Try[EntryData] = Try(a._1)
        }
        val entryData = (EntryData(""), AnotherData(1))
        val invalidNumber = Invalid("Invalid number")
        val r1 = Rule.verify[EntryData](_ => Valid)
        val r2 = Rule.verify[(EntryData, AnotherData)]{ case (_, another) => if(another.n < 2) invalidNumber else Valid}
        assert((r1 combine r2).run(entryData) == Success(entryData, Seq(invalidNumber)))
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
    assert((r1 combine r2 toStep).run(entryData) == Success(entryData, Seq(invalidStringEmpty, invalidStringSize), true))
  }

}
