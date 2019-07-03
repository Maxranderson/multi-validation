package multivalidation.ops

import cats.data.Kleisli
import multivalidation.parsers.{Folder, Parser}
import multivalidation._

import scala.util.{Failure, Success, Try}

trait StepOps[A] { self: CoreTypes with RuleOps =>

  implicit class StepOperator[T](step: Step[T]) {

    /**
      * Create a Step that execute the first Step,
      * execute the second Step if the first step not return invalid results, and combine their results.
      *
      * @param step2 second Step to be combined
      * @param folder Parser for TT to T and combine T and TT types
      * @tparam TT Intermediary Type
      * @return a Step with type TT
      */
    def combine[TT](step2: Step[TT])(implicit folder: Folder[T, TT]): Step[TT] =
      Kleisli { tt: TT =>
        folder.parse(tt).flatMap(step.run) match {
          case Success(value) if value._3 =>
            for {
              reduced <- folder.fold(value._1, tt)
            } yield (reduced, value._2, value._3)
          case Success(value) =>
            for {
              reduced <- folder.fold(value._1, tt)
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
    def toValidation[C](implicit parser: Parser[A, T], parser2: Parser[T, C]): Validation[A, C] =
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
