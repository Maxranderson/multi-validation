package multivalidation.ops

import cats.data.Kleisli
import multivalidation.{CoreTypes, ValidationResult}
import multivalidation.parsers.Parser
import cats.implicits._
import scala.util.{Failure, Try}

trait ValidationOps[A] { self: CoreTypes with StepOps[A] with RuleOps =>

  implicit class ValidationImpl[C](validation: Validation[A, C]) {

    /**
      * Transform Validation to Validator with Failure as default result
      * @return A Validator
      */
    def orNotFound: Validator[A, C] = Kleisli(a => validation.run(a).getOrElse(Failure(new Exception(s"Validation not implemented for $a"))))

    /**
      * Transform Validation to Validator with notFound param as default result
      * @param default Default result
      * @return A Validator
      */
    def orDefaultResult(default: Try[ValidationResult[C]]): Validator[A, C] = Kleisli(a => validation.run(a).getOrElse(default))

    /**
      * Transform Validation to Validator with a default Validation
      * @param default
      * @return
      */
    def orDefault(default: Validation[A, C]): Validator[A, C] = Kleisli(a => validation or default run a getOrElse Failure(new Exception(s"Validation not implemented for $a")))

    private def toKleisli[C](validation: Validation[A, C]): Kleisli[Option, A, Try[ValidationResult[C]]] = validation

    /**
      * Combine with another Validation of the same Final Data
      * @param validation2 Second Validation
      * @return A Validation
      */
    def or(validation2: Validation[A, C]): Validation[A, C] = toKleisli(validation) <+> validation2
  }

  object Validation {

    /**
      * Create a Validation from a Partial Function
      * @param f Partial Function
      * @param parser Parser for Init data to Intermediary data
      * @param parser2 Parser for Intermediary data to Final data
      * @tparam T Intermediary Type
      * @tparam C Final Type
      * @return A Validation
      */
    def caseOf[T, C](f: PartialFunction[A, Step[T, T]])(implicit parser: Parser[A, T], parser2: Parser[T, C]): Validation[A, C] =
      Kleisli(a => f.lift(a).flatMap(step => step.toValidation.run(a)))

    /**
      * Create a Validation from Step based on condition
      * @param cond Condition when Validation is applied
      * @param step Step to create Validation
      * @param parser Parser for Init data to Intermediary data
      * @param parser2 Parser for Intermediary data to Final data
      * @tparam T Intermediary Type
      * @tparam C Final Type
      * @return A Validation
      */
    def ofCond[T, C](cond: A => Boolean)(step: Step[T, T])(implicit parser: Parser[A, T], parser2: Parser[T, C]): Validation[A, C] =
      Kleisli(a => step.toValidationOfCond[C](cond).run(a))

    /**
      * Create a validation from Step
      * @param step Step to create Validation
      * @param parser Parser for Init data to Intermediary data
      * @param parser2 Parser for Intermediary data to Final data
      * @tparam T Intermediary Type
      * @tparam C Final Type
      * @return A Validation
      */
    def of[T, C](step: Step[T, T])(implicit parser: Parser[A, T], parser2: Parser[T, C]): Validation[A, C] =
      Kleisli(a => step.toValidation.run(a))
  }

}
