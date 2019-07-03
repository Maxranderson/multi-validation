package multivalidation.parsers

import scala.util.Try

trait IdFolder {
  implicit def fold[A]: Folder[A, A] = new Folder[A, A] {
   override def fold(a: A, b: A): Try[A] = Try(b)

   override def parse(a: A): Try[A] = Try(a)
  }
}
