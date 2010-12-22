package kata

import scala.util.control._

class BreaksWith[T] { m => 
  private case class BreakWithControl(p: m.type, t: T) extends ControlThrowable
  def breakable(op: => T): T =
    try op catch { case BreakWithControl(n, t) if (m eq n) => t }
  def break[A](t: T): A = 
    throw new BreakWithControl(this, t)
}


object BreaksWith { 
  trait Break[T] {
    def apply[A](t: T): A
  }
  // oneshot call/cc
  def apply[T](f: Break[T] => T): T = {
    var label = new BreaksWith[T]
    var exit = new Break[T] { 
      def apply[A](t: T) = label.break[A](t) 
    }
    label.breakable(f(exit))
  }
}
