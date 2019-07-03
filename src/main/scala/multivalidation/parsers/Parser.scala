package multivalidation.parsers

import scala.util.Try

trait Parser[A, B] {
  def parse(a: A): Try[B]
}