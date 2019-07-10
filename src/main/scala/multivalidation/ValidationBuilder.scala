package multivalidation

import multivalidation.ops._
import multivalidation.parsers.IdParser

trait ValidationBuilder[A] extends CoreTypes
                           with RuleOps
                           with StepOps[A]
                           with ValidationOps[A]
                           with IdParser
