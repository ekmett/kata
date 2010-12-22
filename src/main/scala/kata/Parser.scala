package kata

import scala.util.parsing.combinator.{ LayoutParsers, RegexParsers, JavaTokenParsers }
import scala.util.parsing.input.LayoutPosition
import scala.util.matching.Regex

object Parser extends LayoutParsers {
  override type Elem = Char
  // reverse to cause longer names to come first
  // implicit def set(s: Set[String]): Parser[String] = choice(s.toList.map(string).reverse :_*)
  // implicit def set(s: Set[Char]): Parser[Char] = choice(s.toList.map(char).reverse :_*)

  def keyword = CompressedTrie(
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
    "with"
  )

  def keySymbols = CompressedTrie(
    ":", 
    "->", 
    "<->", 
    "<-", 
    "="
  )

  def tailSymbols = Set('-', '\'','#')

  // def identTailChar: Parser[Char] = more ~> elem(c => Character isLetterOrDigit c || (tailSymbols contains c), "identTail")
  
  // def ident[T](h: Parser[Char], t: Parser[Char], f: String => T]: Parser[T] = h cons rep(t) map f

  // def v: Parser[Var] = """[a-z_]\w*""".r map Var
  // def c: Parser[Con] = """[A-Z_]\w*""".r map Con
  // def vsym: Parser[Sym] = """[ident(symbolChar, symbolTailChar, VarSym)
  // def csym: Parser[Sym] = ident(symbolConChar, symbolTailChar, ConSym)
  // def int: Parser[Int] = wholeNumber map Integer.parseInt
}

