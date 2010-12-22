package kata

class ThreadLocal[T](v: => T) extends java.lang.ThreadLocal[T] with Function0[T] {
  override def initialValue:T = v
  def apply(): T = get()
  def :=(t: T) = set(t)
}

object ThreadLocal { 
  implicit def lower[T](t : ThreadLocal[T]): T = t()
}
