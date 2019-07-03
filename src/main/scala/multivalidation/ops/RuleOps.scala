package multivalidation.ops

import cats.data.Kleisli
import multivalidation.{CoreTypes, Validated}
import cats.implicits._
import multivalidation.parsers.Folder

import scala.util.Try

trait RuleOps {
  self: CoreTypes =>

  implicit class RuleOperator[T](rule: Rule[T]) {

    /**
      * Create a Rule that execute the first Rule,
      * execute the second Rule and combine their results.
      *
      * @param rule2  second Rule to be combined
      * @param folder Parser for TT to T and combine T and TT types
      * @tparam TT Intermediary Type
      * @return a Rule
      */
    def combine[TT](rule2: Rule[TT])(implicit folder: Folder[T, TT]): Rule[TT] =
      Kleisli { tt: TT =>
        for {
          t <- folder.parse(tt)
          r1 <- rule.run(t)
          merged <- folder.fold(r1._1, tt)
          r2 <- rule2.run(merged)
        } yield r2.copy(r2._1, r1._2 ++ r2._2)
      }

    /**
      * Transform a Rule to Step
      * @return A Step with type T
      */
    def toStep: Step[T] = Kleisli { t: T =>
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
    def verify[T](verifier: T => Validated): Rule[T] = build[T](t => (t, verifier(t)))

    /**
      * Create a Rule from verifier
      * @param verifier
      * @tparam T Intermediary Type
      * @return A Rule
      */
    def build[T](verifier: T => (T, Validated)): Rule[T] =
      Kleisli { t: T =>
        Try {
          verifier.andThen(t =>
            t.copy(_2 = if (t._2.isInvalid || t._2.isAlmost) Seq(t._2) else Seq.empty)
          )(t)
        }
      }
  }

}
