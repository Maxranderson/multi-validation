package multivalidation.ops

import cats.data.Kleisli
import multivalidation.parsers.Parser
import multivalidation._

import scala.util.{Failure, Success, Try}

trait StepOps[A] { self: CoreTypes with RuleOps =>

  implicit class StepOperator[T, TT](step: Step[T, TT]) {

    /**
      * Create a Step that execute the first Step,
      * execute the second Step if the first step not return invalid results, and combine their results.
      *
      * @param step2 second Step to be combined
      * @param folder Parser for TT to T and combine T and TT types
      * @tparam AA Intermediary Type
      * @tparam B Intermediary Type Transformed
      * @return a Step with type TT
      */
    def combine[AA, B](step2: Step[AA, TT])(implicit folder: Parser[TT, AA]): Step[T, TT] =
      Kleisli { t: T =>
        step.run(t) match {
          case Success(value) if value._3 => Try(value)
          case Success(value) =>
            for {
              reduced <- folder.parse(value._1)
              r <- step2.run(reduced)
            } yield (r._1, r._2, r._3)
          case Failure(e) => Failure(e)
        }
      }

    /**
      * Transform a Step into a Validation
      * @param parser Parser for Init data to Intermediary data
      * @param parser2 Parser for Intermediary data to Final data
      * @tparam C The final class to be build
      * @return a Validation
      */
    def toValidation[C](implicit parser: Parser[A, T], parser2: Parser[TT, C]): Validation[A, C] =
      Kleisli[Option, A, Try[ValidationResult[C]]] { a =>
        Option {
          for {
            t <- parser.parse(a)
            res <- step.run(t).flatMap {
              case (_, vs, bool) if bool => Success(FailureResult(vs))
              case (t, vs, _) if vs.nonEmpty => parser2.parse(t).map(AlmostResult(vs, _))
              case (t, _, _) => parser2.parse(t).map(SuccessResult.apply)
            }
          } yield res
        }
      }
  }

}
