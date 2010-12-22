package kata

import scala.collection.Traversable
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{ SortedSet, HashMap } 

// TODO: path compression, etc.
class Trie(val root:  Boolean, val children: Map[Char, Trie]) extends SortedSet[String] {
  t => 
  def getChild(char: Char): Trie = children.getOrElse(char, Trie.empty)
  def setChild(char: Char, x: Trie): Trie = new Trie(root, children + (char -> x))
  def modChild(char: Char, f: Trie => Trie) = setChild(char, f(getChild(char)))
  def ordering = implicitly[Ordering[String]]
  def rangeImpl(from: Option[String], to: Option[String]) : SortedSet[String] = { 
    val builder = Trie.newBuilder
    foreach (x => if ((!from.isDefined || from.get <= x) && (!to.isDefined || to.get > x))
        builder += x)
    builder.result
  }

  def contains(k: String): Boolean =
    if (k == "") root
    else children.get(k.charAt(0)) match { 
      case Some(child) => child.contains(k)
      case None => false
    }

  def +(k: String): Trie =
    if (k == "") 
      new Trie(true, children)
    else
      modChild(k.charAt(0), _ + k.substring(1))
  def -(k: String): Trie = 
    if (k == "")
      new Trie(false, children)
    else 
      modChild(k.charAt(0), _ - k.substring(1))
  override def foreach[U](f: String => U) {
    def go(prefix: String, trie: Trie): Unit = {
      if (trie.root) f(prefix)
      trie.children.foreach((kv:(Char,Trie)) => go(prefix+kv._1, kv._2))
    }
    go("", this)
  }
  def iterator: Iterator[String] = toIterator
}

object Trie {
  def apply[A](elems: String*): Trie = {
    val b = newBuilder
    b ++= elems
    b.result
  }
  def unapplySeq[A](trie: Trie): Option[List[String]] = Some(trie.toList)
  type Coll = Trie
  def empty: Trie = newBuilder.result
  def newBuilder: Builder[String, Trie] = new Builder[String, Trie] { 
    var result : Trie = new Trie(false, HashMap.empty)
    def += (elem: String): this.type = {
      result = result + elem
      this
    }
    def clear {
      result = new Trie(false, HashMap.empty)
    }
  }
  class TrieCanBuildFrom extends CanBuildFrom[Trie, String, Trie] {
    def apply(from: Trie) = newBuilder
    def apply() = newBuilder
  }
}
