package kata

import java.lang.Character
import java.lang.Integer
import java.lang.CharSequence
import scala.collection.mutable._
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Position
import scala.util.parsing.input.Reader
import scala.util.parsing.input.CharSequenceReader

sealed trait LayoutContext
case class  IndentedLayout(depth: Int) extends LayoutContext
case object BracedLayout extends LayoutContext

trait LayoutToken 
object LayoutToken { 
  case object VSemi extends LayoutToken {
    override def toString = "virtual semicolon"
  }
  case object VBrace extends LayoutToken {
    override def toString = "virtual right brace"
  }
  case class Other(char: Char) extends LayoutToken { 
    override def toString = 
      if (Character.isLetterOrDigit(char) && char < 128)
        "'" + char + "'"
      else if (char == ' ')
        "whitespace" 
      else 
         "'\\" + Integer.toOctalString(char) + "'"
  }
}

case class LayoutPosition(underlying: Position, layoutStack: List[LayoutContext], bol: Boolean) extends Position {
  def line: Int = underlying.line
  def column: Int = underlying.column
  protected def lineContents: String = ""
  override def longString: String = underlying.longString
  override def toString: String = underlying.toString
  override def <(that: Position): Boolean = that match { 
    case LayoutPosition(underlying2, _, _) => underlying < underlying2
    case _ => underlying.line < that.line ||
              underlying.line == that.line && underlying.column < that.column
  } 
}
trait LayoutParsers extends Parsers {
  import LayoutToken._

  type Elem = Char

  case class LayoutReader(
    underlying: Input,
    layoutStack: List[LayoutContext] = List(IndentedLayout(1)),
    bol: Boolean = false,
    recursionHeads: HashMap[LayoutPosition, Head] = HashMap.empty,
    cache: HashMap[(Parser[_], LayoutPosition), MemoEntry[_]] = HashMap.empty
  ) extends Reader[Char] { outer =>
    def getFromCache[T](p: Parser[T]): Option[MemoEntry[T]] = {
      cache.get((p, pos)).asInstanceOf[Option[MemoEntry[T]]]
    }
    def updateCacheAndGet[T](p: Parser[T], w: MemoEntry[T]): MemoEntry[T] = {
      cache.put((p, pos),w)
      w
    }
    override def source: java.lang.CharSequence = underlying.source
    override def offset: Int = underlying.offset
    var lrStack: List[LR] = Nil
    def pos: LayoutPosition = LayoutPosition(underlying.pos, layoutStack, bol)
    def withLrStack(s: List[LR]) = { 
      lrStack = s
      this
    }
    def first: Char = underlying.first
    def rest = copy (underlying = underlying.rest) withLrStack lrStack
    def atEnd: Boolean = underlying.atEnd
    def top: LayoutContext = layoutStack.head
    def pop: LayoutReader = copy (layoutStack = layoutStack.tail) withLrStack lrStack
    def push(context: LayoutContext): LayoutReader = copy (layoutStack = context :: layoutStack) withLrStack lrStack
    def setBol(bol : Boolean): LayoutReader = copy (bol = bol) withLrStack lrStack
    def depth: Int = layoutStack match { 
      case IndentedLayout(n) :: _ => n
      case _ => 0
    }
  }
  
  override def phrase[T](p: Parser[T]) = {
    val q = super.phrase(p <~ endOfLayout)
    new PackratParser[T] {
      def apply(in: Input) = in match {
        case in: LayoutReader => q(in)
        case in => q(new LayoutReader(in))
      }
    }
  }

  private def endOfLayout: Parser[Unit] = LayoutParser(input => 
    if (input.atEnd && input.layoutStack.length > 1)
      Failure("unfinished layout", input)
    else
      Success((), input)
  )


  private def getPosFromResult(r: ParseResult[_]): Position = r.next.pos
 
  // auxiliary data structures
 
  case class MemoEntry[+T](var r: Either[LR,ParseResult[_]]){
    def getResult: ParseResult[T] = r match {
      case Left(LR(res,_,_)) => res.asInstanceOf[ParseResult[T]]
      case Right(res) => res.asInstanceOf[ParseResult[T]]
    }
  }
  
  case class LR(var seed: ParseResult[_], var rule: Parser[_], var head: Option[Head]){
    def getPos: Position = getPosFromResult(seed)
  }
  
  case class Head(var headParser: Parser[_], var involvedSet: List[Parser[_]], var evalSet: List[Parser[_]]){
    def getHead = headParser
  }
  
  /** 
   * The root class of packrat parsers. 
   */
  abstract class PackratParser[+T] extends Parser[T]
  
  /**
   * Implicitly convert a parser to a packrat parser.
   * The conversion is triggered by giving the appropriate target type: 
   * val myParser: PackratParser[MyResult] = aParser
   */
  implicit def parser2packrat[T](p: => super.Parser[T]): PackratParser[T] = {
    lazy val q = p
    memo(super.Parser {in => q(in)})
  }

  /*
   * An unspecified function that is called when a packrat reader is applied.
   * It verifies whether we are in the process of growing a parse or not. 
   * In the former case, it makes sure that rules involved in the recursion are evaluated. 
   * It also prevents non-involved rules from getting evaluated further
   */
  
  private def recall(p: super.Parser[_], in: LayoutReader): Option[MemoEntry[_]] = {
    val cached = in.getFromCache(p)
    val head = in.recursionHeads.get(in.pos)
    
    head match {
      case None => /*no heads*/ cached
      case Some(h@Head(hp, involved, evalSet)) => {
        //heads found
        if(cached == None && !(hp::involved contains p)) {
          //Nothing in the cache, and p is not involved
          return Some(MemoEntry(Right(Failure("dummy ",in))))
        }
        if(evalSet contains p){
          //something in cache, and p is in the evalSet
          //remove the rule from the evalSet of the Head
          h.evalSet = h.evalSet.filterNot(_==p)
          val tempRes = p(in)
          //we know that cached has an entry here
          val tempEntry: MemoEntry[_] = cached.get // match {case Some(x: MemoEntry[_]) => x}
          //cache is modified
          tempEntry.r = Right(tempRes)
        }
        cached
      }
    }
  }
  
  /*
   * setting up the left-recursion. We have the LR for the rule head
   * we modify the involvedSets of all LRs in the stack, till we see
   * the current parser again
   */
  private def setupLR(p: Parser[_], in: LayoutReader, recDetect: LR): Unit = {
    if(recDetect.head == None) recDetect.head = Some(Head(p, Nil, Nil))
    
    in.lrStack.takeWhile(_.rule != p).foreach {x =>
      x.head = recDetect.head
      recDetect.head.map(h => h.involvedSet = x.rule::h.involvedSet)
    }
  }
  
  /*
   * growing, if needed the recursion
   * check whether the parser we are growing is the head of the rule.
   * Not => no grow
   */
   
  /*
   * Once the result of the recall function is known, if it is nil, then we need to store a dummy
failure into the cache (much like in the previous listings) and compute the future parse. If it
is not, however, this means we have detected a recursion, and we use the setupLR function
to update each parser involved in the recursion.
   */
  
  private def lrAnswer[T](p: Parser[T], in: LayoutReader, growable: LR): ParseResult[T] = growable match {
    //growable will always be having a head, we can't enter lrAnswer otherwise
    case LR(seed ,rule, Some(head)) => 
      if(head.getHead != p) /*not head rule, so not growing*/ seed.asInstanceOf[ParseResult[T]]
      else {
        in.updateCacheAndGet(p, MemoEntry(Right[LR, ParseResult[T]](seed.asInstanceOf[ParseResult[T]])))
        seed match {
          case f@Failure(_,_) => f
          case e@Error(_,_) => e
          case s@Success(_,_) => /*growing*/ grow(p, in, head)
        }
      }
    case _=> throw new Exception("lrAnswer with no head !!")
  }

  //p here should be strict (cannot be non-strict) !!
  //failing left-recursive grammars: This is done by simply storing a failure if nothing is found

  /** 
   * Explicitly convert a given parser to a memoizing packrat parser.
   * In most cases, client code should avoid calling <code>memo</code> directly
   * and rely on implicit conversion instead.
   */
  def memo[T](p: super.Parser[T]): PackratParser[T] = {
    new PackratParser[T] {
      def apply(in: Input) = {
        /*
         * transformed reader
         */
        val inMem = in.asInstanceOf[LayoutReader]
        
        //look in the global cache if in a recursion
        val m = recall(p, inMem)
        m match {
          //nothing has been done due to recall
          case None =>
            val base = LR(Failure("Base Failure",in), p, None)
            inMem.lrStack = base::inMem.lrStack
            //cache base result
            inMem.updateCacheAndGet(p,MemoEntry(Left(base)))
            //parse the input
            val tempRes = p(in)
            //the base variable has passed equality tests with the cache
            inMem.lrStack = inMem.lrStack.tail
            //check whether base has changed, if yes, we will have a head
            base.head match {
              case None => 
                /*simple result*/
                inMem.updateCacheAndGet(p,MemoEntry(Right(tempRes)))
                tempRes
              case s@Some(_) =>
                /*non simple result*/
                base.seed = tempRes
                //the base variable has passed equality tests with the cache
                val res = lrAnswer(p, inMem, base)
                res
            }
            
          case Some(mEntry) => {
            //entry found in cache
            mEntry match {
              case MemoEntry(Left(recDetect)) => {
                setupLR(p, inMem, recDetect)
                //all setupLR does is change the heads of the recursions, so the seed will stay the same
                recDetect match {case LR(seed, _, _) => seed.asInstanceOf[ParseResult[T]]}
              }
              case MemoEntry(Right(res: ParseResult[_])) => res.asInstanceOf[ParseResult[T]]
            }
          }
        }
      }
    }
  } 
  
  private def grow[T](p: super.Parser[T], rest: LayoutReader, head: Head): ParseResult[T] = {
    //store the head into the recursionHeads
    rest.recursionHeads.put(rest.pos, head /*match {case Head(hp,involved,_) => Head(hp,involved,involved)}*/)
    val oldRes: ParseResult[T] = rest.getFromCache(p).get match {
      case MemoEntry(Right(x)) => x.asInstanceOf[ParseResult[T]]
      case _ => throw new Exception("impossible match")
    }

    //resetting the evalSet of the head of the recursion at each beginning of growth
    head.evalSet = head.involvedSet
    val tempRes = p(rest); tempRes match {
      case s@Success(_,_) => 
        if(getPosFromResult(oldRes) < getPosFromResult(tempRes)) {
          rest.updateCacheAndGet(p, MemoEntry(Right(s)))
          grow(p, rest, head)
        } else {
          //we're done with growing, we can remove data from recursion head
          rest.recursionHeads -= rest.pos
          rest.getFromCache(p).get match {
            case MemoEntry(Right(x: ParseResult[_])) => x.asInstanceOf[ParseResult[T]]
            case _ => throw new Exception("impossible match")
          }
        }
      case f => 
        rest.recursionHeads -= rest.pos
        /*rest.updateCacheAndGet(p, MemoEntry(Right(f)));*/oldRes
    }
  }

  private def newline: Parser[Char] = elem("newline", _ == '\n')
  private def anyChar: Parser[Char] = more ~> elem("any", _ => true)
  private def realSpace: Parser[Char] = elem("whiteSpace", Character.isWhitespace)

  def LayoutParser[T](f: LayoutReader => ParseResult[T]): Parser[T] =
    Parser(i => f(i.asInstanceOf[LayoutReader]))

  private def adjustLayout(f: LayoutReader => LayoutReader): Parser[Unit] =
    LayoutParser(input => Success((), f(input)))

  private def setBol(bol: Boolean): Parser[Unit] = adjustLayout(_.setBol(bol))

  private def pushContext(ctx: LayoutContext): Parser[Unit] = adjustLayout(_.push(ctx))

  private def popContext(msg: String, f: LayoutContext => Boolean): Parser[Unit] = LayoutParser(input => 
    if (f(input.top)) 
      Success((), input.pop)
    else
      Failure(msg, input)
  )

  implicit def string(s: String) : Parser[String] = 
    (success(()) /: s)((s: Parser[Unit], c: Char) => s <~ char(c)) ~> success(s)

  implicit def char(c: Char) : Parser[Char] = elem(c)

  private object Layout { 
    def nested(side: Boolean): Parser[Boolean] = 
      ("-}" ~> success(side)) | 
      (string("{-") ~> (nested(side) flatMap nested)) | 
      (newline ~> nested(true)) | 
      (anyChar ~> nested(side))
    def comment: Parser[LayoutToken] 
      = string("--") ~> rep(not(newline)) ~> newline ~> whiteSpace(true, true)
    def realWhitespace = rep1(elem("spaces", java.lang.Character.isWhitespace))
    def whiteSpace(spaced: Boolean, side: Boolean): Parser[LayoutToken] = 
      (string("{-") ~> (nested(side).flatMap(k => whiteSpace(true, k)))) | 
      comment |
      (char('\n') ~> whiteSpace(true, true)) | 
      (realWhitespace ~> whiteSpace(true, side)) |
      (if (side) offside(spaced) else onside(spaced))
    def offside(spaced: Boolean): Parser[LayoutToken] = LayoutParser(input => { 
      val layoutDepth = input.depth
      val col = input.pos.column
      if (col < layoutDepth) { 
        Success(VBrace, input.pop.setBol(true))
      } else if (col == layoutDepth) { 
        Success(VSemi, input)
      } else {
        onside(spaced)(input)
      }
    })
    def onside(spaced: Boolean): Parser[LayoutToken] = 
      if (spaced) {
        success(Other(' '))
      } else { 
        setBol(false) ~> (trailingVBrace | anyChar.map(Other(_)))
      }
    def getBol: Parser[Boolean] = LayoutParser(input => Success(input.bol, input))
  }

  def more: Parser[Unit] = LayoutParser(input => 
    if (input.atEnd)
      Failure("eof", input)
    else
      Success((), input)
  )

  def layout: Parser[LayoutToken] = memo(Layout.getBol flatMap (bol => Layout.whiteSpace(false, bol)))

  def trailingVBrace: Parser[LayoutToken] = LayoutParser(input => {
      if (input.atEnd && input.layoutStack.length > 1 && input.top.isInstanceOf[IndentedLayout])
        Success(VBrace,input.pop)
      else
        Failure("expected virtual right brace", input)
  })
    
  def virtualLeftBrace: Parser[Unit] = LayoutParser(input =>  
    Success((), input.push(IndentedLayout(input.pos.column max input.depth)))
  ) named "indentation"

  def virtualRightBrace: Parser[Unit] = layout ^? ({ case VBrace => () }, s => "expected outdent, not " + s)

  def semi: Parser[Char] = layout ^? ({ 
    case VSemi => ';'
    case Other(';') => ';'
  }, s => "expected semi-colon, not " + s)

  def leftBrace: Parser[Char] = '{' <~ pushContext(BracedLayout)
  def rightBrace: Parser[Char] = popContext("expected right brace", _ == BracedLayout) ~> '}'

  def space: Parser[Char] = layout ^? ({ case Other(' ') => ' ' }, s => "expected space, not " + s)

  def spaced[T](p : Parser[T]) = p <~ opt(space)

  def laidout[T](p: Parser[T]): Parser[List[T]] = {
    (spaced(leftBrace) ~> repsep(p, spaced(';')) <~ spaced(rightBrace)) | 
    (spaced(virtualLeftBrace) ~> repsep(p, spaced(semi)) <~ spaced(virtualRightBrace))
  }

  def parse[T](p: Parser[T], in: Reader[Char]): ParseResult[T] = p(LayoutReader(in))
  def parse[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] = p(LayoutReader(new CharSequenceReader(in)))
  // def parse[T](p: Parser[T], in: java.io.Reader): ParseResult[T] = p(LayoutReader(new PagedSeqReader(PagedSeq.fromReader(in))))

  def parseAll[T](p: Parser[T], in: Reader[Char]): ParseResult[T] = parse(phrase(p), in)
  def parseAll[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] = parse(phrase(p), in)
  // def parseAll[T](p: Parser[T], in: java.io.Reader): ParseResult[T] = parse(phrase(p), in)
}
