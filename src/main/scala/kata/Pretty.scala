package kata

trait Pretty { 
  def pretty(out: StringBuilder)
  override def toString: String = Pretty.print(this)
}

object Pretty { 
  def print[T<:Pretty](t:T): String = {
    val buffer = new StringBuilder
    t.pretty(buffer)
    buffer.toString
  }
}
