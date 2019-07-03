package multivalidation

sealed abstract class ValidationResult[+C]

case class SuccessResult[C](result: C) extends ValidationResult[C]

case class AlmostResult[C](validated: Seq[Validated], result: C) extends ValidationResult[C]

case class FailureResult(validated: Seq[Validated]) extends ValidationResult[Nothing]