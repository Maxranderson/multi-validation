package multivalidation

sealed abstract class Validated {
  def isInvalid: Boolean

  def isAlmost: Boolean

  def isValid: Boolean
}

case object Valid extends Validated {
  override def isInvalid: Boolean = false

  override def isAlmost: Boolean = false

  override def isValid: Boolean = true
}

class Almost(val msg: String) extends Validated {
  override def isInvalid: Boolean = false

  override def isAlmost: Boolean = true

  override def isValid: Boolean = false
}

object Almost {
  def apply(msg: String): Almost = new Almost(msg)
}

class Invalid(val msg: String) extends Validated {
  override def isInvalid: Boolean = true

  override def isAlmost: Boolean = false

  override def isValid: Boolean = false
}

object Invalid {
  def apply(msg: String): Invalid = new Invalid(msg)
}