package multivalidation.parsers

import scala.util.Try

trait IdParser {
  implicit def fold[A]: Parser[A, A] = new Parser[A, A] {
   override def parse(a: A): Try[A] = Try(a)
  }
}
