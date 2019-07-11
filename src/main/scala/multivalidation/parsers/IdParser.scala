package multivalidation.parsers

import scala.util.Try

trait IdParser {
  implicit def parser[A]: Parser[A, A] = new Parser[A, A] {
   override def parse(a: A): Try[A] = Try(a)
  }

  implicit def parser2[A]: Parser[(A, A), A] = new Parser[(A, A), A] {
    override def parse(a: (A, A)): Try[A] = Try(a._1)
  }
}
