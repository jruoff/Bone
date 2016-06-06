/** Implementation of the ternary logic of Kleene. */
final case class Kleene private(private val value: Int) {

  /** Short circuiting Kleene AND */
  def &&(other: => Kleene): Kleene =
    if (this == Kleene.False)
      Kleene.False
    else
      new Kleene(value min other.value)

  /** Short circuiting Kleene OR */
  def ||(other: => Kleene): Kleene =
    if (this == Kleene.True)
      Kleene.True
    else
      new Kleene(value max other.value)

  /** Kleene NOT */
  def unary_!(): Kleene = new Kleene(-value)

  /** Kleene XOR */
  def ^(other: Kleene): Kleene =
    new Kleene(value * -other.value)

  /** Convert into an optional boolean */
  def boolean: Option[Boolean] =
    if (this == Kleene.Unknown) None
    else Some(this == Kleene.True)

  /** Convert into a boolean */
  def boolean(default: Boolean): Boolean =
    boolean.getOrElse(default)

  /** Convert to string */
  override def toString: String = this match {
    case Kleene.True    => "true"
    case Kleene.Unknown => "unknown"
    case Kleene.False   => "false"
  }
}

object Kleene {
  /** Kleene True */
  val True    = new Kleene(+1)

  /** Kleene Unknown */
  val Unknown = new Kleene( 0)

  /** Kleene False */
  val False   = new Kleene(-1)

  import play.api.libs.json._

  /** Implicit object to write a Kleene instance to JSON.
    * True is true, False is false, and Unknown is null.
    */
  implicit object IntWrites extends Writes[Kleene] {
    def writes(o: Kleene): JsValue =
      o.boolean.map(JsBoolean).getOrElse(JsNull)
  }
}
