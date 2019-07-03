package multivalidation.parsers

import scala.util.Try

trait Folder[A, B] extends Parser[B, A] {
  def fold(a: A, b: B): Try[B]
}
