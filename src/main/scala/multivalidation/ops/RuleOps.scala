package multivalidation.ops

import cats.data.Kleisli
import multivalidation.{CoreTypes, Validated}
import cats.implicits._
import multivalidation.parsers.Parser

import scala.util.Try

trait RuleOps {
  self: CoreTypes =>

  implicit class RuleOperator[T, TT](rule: Rule[T, TT]) {

    /**
      * Create a Rule that execute the first Rule,
      * execute the second Rule and combine their results.
      *
      * @param rule2  second Rule to be combined
      * @param parser Parser for TT to A
      * @tparam A Intermediary Type
      * @tparam B Intermediary Type Transformed
      * @return a Rule
      */
    def combine[A, B](rule2: Rule[A, B])(implicit parser: Parser[TT, A]): Rule[T, B] =
      Kleisli { t: T =>
        for {
          r1 <- rule.run(t)
          parsed <- parser.parse(r1._1)
          r2 <- rule2.run(parsed)
        } yield (r2._1, r1._2 ++ r2._2)
      }

    /**
      * Transform a Rule to Step
      * @return A Step with type T
      */
    def toStep: Step[T, TT] = Kleisli { t: T =>
      for {
        r <- rule.run(t)
      } yield (r._1, r._2, r._2.exists(_.isInvalid))
    }
  }

  object Rule {

    /**
      *
      * @param run Code to be delayed
      * @tparam T Type for the rule
      * @tparam B Type delayed
      * @return a Kleisli with the delayed code
      */
    def delay[T, B](run: => B): Kleisli[Try, T, B] = Kleisli[Try, T, B] { _ =>
      Try(run)
    }

    /**
      * Create a Rule from verifier
      * @param verifier Function that will receive the Intermediary data and return a Validated result
      * @tparam T Intermediary Type
      * @return A Rule
      */
    def verify[T](verifier: T => Validated): Rule[T, T] = build[T](t => (t, verifier(t)))

    /**
      * Create a Rule from verifier
      * @param verifier
      * @tparam T Intermediary Type
      * @return A Rule
      */
    def build[T](verifier: T => (T, Validated)): Rule[T, T] =
      Kleisli { t: T =>
        Try {
          verifier.andThen(t =>
            t.copy(_2 = if (t._2.isInvalid || t._2.isAlmost) Seq(t._2) else Seq.empty)
          )(t)
        }
      }
  }

}
