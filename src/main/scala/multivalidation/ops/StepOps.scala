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
      * @param parser Parser for TT to C
      * @param parser2 Parser for (D, TT) to TT
      * @tparam C Intermediary Type
      * @tparam D Intermediary Type
      * @return a Step with the same left type
      */
    def <+[C, D](step2: Step[C, D])(implicit parser: Parser[TT, C], parser2: Parser[(D, TT), TT]): Step[T, TT] =
      Kleisli { t: T =>
        step.run(t) match {
          case Success(value) =>
            value._1.map { v1 =>
              for {
                c <- parser.parse(v1)
                r <- step2.run(c)
                res <- r._1.map { d =>
                  parser2.parse((d, v1)).map(tt => (Option(tt), r._2))
                } getOrElse {
                  Try((Option.empty[TT], r._2))
                }
              } yield res
            } getOrElse {
              Try((Option.empty[TT], value._2))
            }
          case Failure(e) => Failure(e)
        }
      }

    def ++[C, D](step2: Step[C, D])(implicit parser: Parser[TT, C]): Step[T, D] =
      Kleisli { t: T =>
        step.run(t) match {
          case Success(value) =>
            value._1.map { v1 =>
              for {
                reduced <- parser.parse(v1)
                r <- step2.run(reduced)
              } yield r
            } getOrElse {
              Try((Option.empty[D], value._2))
            }
          case Failure(e) => Failure(e)
        }
      }

    /**
      * Create a Step that execute the first Step,
      * execute the second Step if the first step not return invalid results, and combine their results.
      *
      * @param step2 second Step to be combined
      * @param parser Parser for TT to T and combine T and TT types
      * @tparam C Intermediary Type
      * @tparam D Intermediary Type
      * @return a Step with the same right type
      */
    def +>[C, D](step2: Step[C, D])(implicit parser: Parser[C, T], parser3: Parser[(TT, C), C]): Step[C, D] =
      Kleisli { c: C =>
        parser.parse(c).flatMap(step.run) match {
          case Success(value) =>
            value._1.map { v1 =>
              for {
                c <- parser3.parse((v1, c))
                res <- step2.run(c)
              } yield res
            } getOrElse {
              Try((Option.empty[D], value._2))
            }
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
      toValidationOfCond(_ => true)

    /**
      * Transform a Step into a Validation
      * @param parser Parser for Init data to Intermediary data
      * @param parser2 Parser for Intermediary data to Final data
      * @tparam C The final class to be build
      * @return a Validation
      */
    def toValidationOfCond[C](cond: A => Boolean)(implicit parser: Parser[A, T], parser2: Parser[TT, C]): Validation[A, C] =
      Kleisli[Option, A, Try[ValidationResult[C]]] { a =>
        Option(a).filter(cond).map { a =>
          for {
            t <- parser.parse(a)
            res <- step.run(t).flatMap {
              case (Some(t), vs) if vs.nonEmpty => parser2.parse(t).map(AlmostResult(vs, _))
              case (Some(t), _) => parser2.parse(t).map(SuccessResult.apply)
              case (None, vs) => Success(FailureResult(vs))
            }
          } yield res
        }
      }
  }

}
