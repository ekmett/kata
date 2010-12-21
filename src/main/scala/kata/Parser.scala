package kata

import scala.util.parsing.combinator.LayoutParsers
import scala.util.parsing.input.LayoutPosition

object Parser extends LayoutParsers {
  // reverse to cause longer names to come first
  implicit def set(s: Set[String]): Parser[String] = choice(s.toList.map(string).reverse :_*)
  implicit def set(s: Set[Char]): Parser[Char] = choice(s.toList.map(char).reverse :_*)

  def keyword = Trie(
    "case",
    "class",
    "data",
    "ensures",
    "field",
    "import",
    "in",
    "inside",
    "let",
    "of",
    "open",
    "private",
    "public",
    "protected",
    "unifies",
    "with",
  )

  def keySymbols = Trie(
    ":", 
    "->", 
    "<->", 
    "<-", 
    "="
  )

  def tailSymnols = Set('-', '\'','#')

  def identTailChar: Parser[Char] =
    more ~> elem(c => Character isLetterOrDigit c || (tailSymbols contains c), "identTail")
  
  def ident[T](h: Parser[Char], t: Parser[Char], f: String => T]: Parser[T] =
    h cons rep(t) map f

  def v: Parser[Var] = ident(lowerChar, identTailChar, Var)
  def c: Parser[Var] = ident(upperChar, identTailChar, Con)
  def vsym: Parser[Sym] = ident(symbolChar, symbolTailChar, VarSym)
  def csym: Parser[Sym] = ident(symbolConChar, symbolTailChar, ConSym)

}

