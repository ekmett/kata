package kata

class Fixity(val fixity: Int, val fixityDirection: Fixity.Direction)
object Fixity {
  type Direction = Direction.Value
  object Direction extends Enumeration {
    val InfixL, InfixR, InfixN = Value
  }
  def apply(fixity: Int, fixityDirection: Direction): Fixity = new Fixity(fixity, fixityDirection)
  def unapply(f: Fixity): Option[(Int, Direction)] = Some((f.fixity, f.fixityDirection))
  def default: Fixity = Fixity(9, Fixity.Direction.InfixL)
  def maxPrecedence: Int = 9
}
