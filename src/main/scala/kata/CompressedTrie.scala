package kata

import scala.collection.Traversable
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{ SortedSet, HashMap } 
import scala.util.parsing.combinator.RegexParsers

trait CompressedTrie extends SortedSet[String] {
  import Parser._
  def ordering = implicitly[Ordering[String]]
  def +(n: String): CompressedTrie
  def -(n: String): CompressedTrie
  override def empty: CompressedTrie = CompressedTrie.empty
  def prepend(k: String): CompressedTrie 
  def parse: Parser[String]
}

object CompressedTrie { 
  import Parser._
  object empty extends CompressedTrie {
    def contains(k: String): Boolean = false
    override def size = 0
    def +(n: String): CompressedTrie = new Singleton(n)
    def -(k: String): CompressedTrie = this
    override def foreach[U](f: String => U) {}
    def iterator: Iterator[String] = Iterator.empty
    def prepend(k: String): CompressedTrie = empty
    def rangeImpl(from: Option[String], to: Option[String]) : SortedSet[String] = empty
    // def parseWith[P<:RegexParsers](p : P) : P#Parser[String] = p.failure("empty")
    def parse: Parser[String] = failure("empty")
  }
  class Singleton(val m: String) extends CompressedTrie {
    def contains(n: String): Boolean = m == n
    override def size = 1
    def +(n: String): CompressedTrie =
      if (m == n) this
      else {
        val len = m.length min n.length
        val k = 0 to (len - 1) find (i => m.charAt(i) != n.charAt(i)) getOrElse len
        val root = m.substring(0,k)
        val nIsShort = n.length == k 
        val mIsShort = m.length == k
        val builder : Builder[(Char, CompressedTrie), HashMap[Char, CompressedTrie]] = HashMap.newBuilder
        if (!mIsShort) builder += (m.charAt(k) -> new Singleton(m.substring(k + 1)))
        if (!nIsShort) builder += (n.charAt(k) -> new Singleton(n.substring(k + 1)))
        new Branch(root, nIsShort || mIsShort, 2, builder.result)
      }
    def -(n: String): CompressedTrie = if (m == n) empty else this
    def prepend(k: String): Singleton = new Singleton(k + m)
    override def foreach[U](f: String => U) { f(m) }
    def iterator: Iterator[String] = Iterator.single[String](m)
    def rangeImpl(from: Option[String], to: Option[String]) : SortedSet[String] = 
      if ((!from.isDefined || from.get <= m) && (!to.isDefined || to.get > m)) this
      else empty
    def parse: Parser[String] = literal(m)
  }

  class Branch(
    val m: String,
    val present: Boolean,
    override val size: Int,
    val children: Map[Char, CompressedTrie]
  ) extends CompressedTrie { 
    def prepend(n: String): Branch = 
      new Branch(n + m, present, size, children)
    def contains(n: String) = {
      val k = m.length
      (n startsWith m) && (
        if (k == n.length)
          present
        else children.get(n.charAt(k)) match { 
          case Some(child) => child.contains(n.substring(k + 1))
          case None => false
        }
      )
    }
    def -(n: String): CompressedTrie = 
      if (!contains(n))
        this // nothing to do 
      else if (size == 2)
        new Singleton(find (_ != n).get) // collapse
      else { 
        var k = m.length

        if (k == n.length) {
          // our prefix has the same length as the target, so m == n, and present == true
          if (children.size == 1) {
            var kv = children.head
            kv._2.prepend(m + kv._1)
          } else new Branch(m,false,size - 1,children)
        } else {
          var c = n.charAt(k)
          new Branch(m,present,size-1,children + (c -> (children(c) - n.substring(k + 1))))
        }
      }
    def +(n: String): CompressedTrie = 
      if (contains(n)) this
      else if (m == n) new Branch(m, true, size + 1, children)
      else {
        val len = m.length min n.length
        val k = 0 to (len - 1) find (i => m.charAt(i) != n.charAt(i)) getOrElse len
        if (k < m.length) new Branch(m.substring(0,k), true, size + 1, HashMap(
            (m.charAt(k) -> new Branch(m.substring(k+1),present,size,children)),
            (n.charAt(k) -> new Singleton(n.substring(k+1)))
          ))
        else new Branch(m, present, size + 1, { 
          val nk = n.charAt(k)
          children + (nk -> (children.getOrElse(nk, empty) + n.substring(k + 1)))
        })
      }
    override def foreach[U](f: String => U) { 
      if (present) f(m)
      children.foreach((kv:(Char,CompressedTrie)) => kv._2.foreach(k => f(m + kv._1.toString + k)))
    }
    def iterator: Iterator[String] = { 
      val result = children.toIterator.flatMap (kv => kv._2.map(x => m + kv._1 + x).toIterator)
      if (present) Iterator.single(m) ++ result
      else result
    }
    // TODO: something faster
    def rangeImpl(from: Option[String], to: Option[String]) : SortedSet[String] = { 
      val builder = CompressedTrie.newBuilder
      foreach (x => if ((!from.isDefined || from.get <= x) && (!to.isDefined || to.get > x))
        builder += x
      )
      builder.result
    }
    def parse: Parser[String] = {
      def oneOf[A](xs: Iterable[Parser[A]]): Parser[A] = 
        (xs :\(err("no match"):Parser[A]))(_ | _)
      literal(m) ~> oneOf(
        children.map(kv => elem(kv._1) ~ commit(kv._2.parse))
      ).map(cs => m + cs._1 + cs._2)
      // TODO: handle 'present' when this starts to work
    }
  }

  def apply[A](elems: String*): CompressedTrie = {
    val b = newBuilder
    b ++= elems
    b.result
  }
  def unapplySeq[A](trie: CompressedTrie): Option[List[String]] = Some(trie.toList)
  type Coll = CompressedTrie
  def newBuilder: Builder[String, CompressedTrie] = new Builder[String, CompressedTrie] { 
    var result : CompressedTrie = empty
    def += (elem: String): this.type = {
      result = result + elem
      this
    }
  /*
    override def ++= (xs: TraversableOnce[String]): this.type = {
      xs match { 
      case x : CompressedTrie if result.size == 0 => result = x
      case _ => xs.foreach (result += _)
      }
      this
    }
  */
    def clear {
      result = empty
    }
  }
  class CompressedTrieCanBuildFrom extends CanBuildFrom[TraversableOnce[String], String, CompressedTrie] {
    def apply(from: TraversableOnce[String]) = newBuilder
    def apply() = newBuilder
  }
  implicit def canBuildFrom: CanBuildFrom[TraversableOnce[String], String, CompressedTrie] = new CompressedTrieCanBuildFrom
}
