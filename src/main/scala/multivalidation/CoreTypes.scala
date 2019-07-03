package multivalidation

import cats.{FlatMap, Functor}
import cats.data.Kleisli
import cats.implicits._
import scala.util.Try

trait CoreTypes {

  /**
    * The Rule Type that will validated the Intermediary Data
    * @tparam T Intermediary Type
    */
  type Rule[T] = Kleisli[Try, T, (T, Seq[Validated])]

  /**
    * The Step Type that represents a step of validation
    * @tparam T
    */
  type Step[T] = Kleisli[Try, T, (T, Seq[Validated], Boolean)]

  /**
    * A Validation that can be validate the Init Data and result the Final Data
    * @tparam A Init Data
    * @tparam C Final Data
    */
  type Validation[A, C] = Kleisli[Option, A, Try[ValidationResult[C]]]

  /**
    * A Validator that validate the Entry Data and return the Final Data
    * @tparam A
    * @tparam C
    */
  type Validator[A, C] = Kleisli[Try, A, ValidationResult[C]]

  implicit val tryFunctor: Functor[Try] = implicitly[Functor[Try]]
  implicit val tryFlatmap: FlatMap[Try] = implicitly[FlatMap[Try]]

}
