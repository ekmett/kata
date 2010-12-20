package kata

import org.kiama.attribution.Attributable
import scala.util.parsing.input.Positional

sealed trait Syntax extends Positional with Attributable
sealed trait Rhs extends Syntax 
sealed trait Pat extends Syntax // patterns
sealed trait Stm extends Syntax // statements (do block contents)
sealed trait Exp extends Rhs with Stm // expressions
sealed trait Dec extends Syntax // declarations
sealed trait IrrPat extends Pat // irrefutable patterns

sealed trait QName extends Syntax // (potentially) qualified name
sealed trait Name extends QName // unqualified name

sealed trait QVar extends QName
case class Var(val name: String) extends QVar with Name with Exp with IrrPat // foo
case class DotVar(path: Exp, member: Var) extends QVar with Exp // (...).foo
case class QuotedVarSym(quoted: VarSym) extends QVar with Name with IrrPat

sealed trait QCon extends QName
case class Con(val name: String) extends QCon with Name with Exp // Foo
case class DotCon(path: Exp, member: Con) extends QCon with Exp // (...).Foo
case class QuotedConSym(quoted: ConSym) extends QCon with Name with Exp

sealed trait QSym extends Syntax
sealed trait QConSym extends QSym
sealed trait QVarSym extends QSym
sealed trait Sym extends QSym

case class VarSym(val name: String) extends QVarSym with Sym    // +
case class SubVarSym(symbol: Sym, path: Exp) extends QVarSym    // +_(...)
case class QuotedVar(quoted: Var) extends QVarSym with Sym      // `foo`

case class ConSym(val name: String) extends QConSym with Sym    // :+
case class SubConSym(symbol: ConSym, path: Exp) extends QConSym // :+_(...)
case class QuotedCon(quoted: Con) extends QConSym with Sym      // `Foo`

sealed abstract class Literal[T](val value: T) extends Exp with Pat
case class IntLiteral(override val value: Int) extends Literal[Int](value)
case class StringLiteral(override val value: String) extends Literal[String](value)

case class InfixSection(lhs: Option[Exp], sym: QSym, rhs: Option[Exp]) extends Exp

case class Ap(lhs: Exp, rhs: Exp) extends Exp        // (...) (...)
case class ApPat(qcon: QCon, pats: Pat*) extends Pat // Foo x y z

case class Case(body: Exp, alts: Alt*) extends Exp
case class Alt(pattern: Pat, rhs: Rhs, where: Dec*) extends Syntax
case class GuardedRhs(alts: List[GuardedAlt]) extends Rhs
case class GuardedAlt(guards: List[Exp], body: Exp) extends Syntax

case class Do(body: Stm*) extends Exp

case class InfixApPat(lhs: Pat, qconsym: QConSym, rhs: Pat) extends Pat // x :+_(...) y

case class PAsPat(lhs: Pat, rhs: Pat) extends Pat // x@y
case object Underscore extends IrrPat // _

case class Adjective(val name: String) extends Syntax
case class Noun(val name: String) extends Syntax
case class CName(adjs: List[Adjective], noun: Noun) extends Syntax

sealed trait Data[T<:Syntax] extends Syntax
case class PrefixData[T<:Syntax](con: Con, fields: List[T]) extends Data[T]
case class InfixData[T<:Syntax](lhs: T, operator: ConSym, rhs: T) extends Data[T]

case class ClassDec(aliases: List[CName], supers: List[CName], self: Option[Var], sections: Section*) extends Dec
case class DataDec(head: Data[Name], supers: List[CName], self: Option[Var], sections: Section*) extends Dec
case class FixityDec(fixity: Fixity, identifier: Sym*) extends Dec

sealed trait Sig extends Syntax
case class SigVar(context: Option[Sig], head: Var, args: IrrPat*) extends Sig    // sig _ _ .bar _ _
case class SigSym(lhs: IrrPat, head: VarSym, context: Option[Sig], rhs: IrrPat) extends Sig // _ +_(_ + _) _

case class SigDec(sigTerm: List[Sig], sigType: CName) extends Dec // foo 
case class FunBind(matches: Match*) extends Dec // foo x y z = ; _ + _ = ...

case class Match(fun: Name, args: List[Pat], rhs: Rhs, where: Dec*) extends Dec // foo x y z = ; _ + _ = ...
case class PatBind(pat: Pat, rhs: Rhs, where: Dec*) extends Dec // Foo x y = .. ; x :+ y = ...

case class Lambda(pats: List[Pat], body: Exp) extends Exp

sealed trait OpenableExp extends Exp
case class SigExp(exp: Exp, sigType: CName) extends OpenableExp // automatically narrows the members of exp to a given type signature?
case class DataExp(data: Data[Exp]) extends OpenableExp         // when a data declaration is in unqualified scope, we know its class and members

sealed trait InLhs extends Syntax
case class Open(body: OpenableExp) extends Dec with Stm with InLhs
case class Let(decls: Dec*) extends Stm with Dec with InLhs

case class InExp(lhs: InLhs, in: Exp) extends Exp // let ... in exp
case class InPat(lhs: InLhs, in: Pat) extends Pat // open ... in Foo
case class Inside(lhs: InLhs, inside: Dec*) extends Dec
case class ViewPat(lhs: Exp, rhs: Pat) extends Pat // bar (foo 12 -> OK x) = ...

sealed trait Prot
case object Public extends Prot
case object Protected extends Prot
case object Private extends Prot
case object With extends Prot

sealed trait Section extends Syntax
case class ProtSection(prot: Prot, decls: Dec*) extends Section
case class UnifiesSection(unifications: Unification*) extends Section
case class EnsuresSection(rules: Rule*) extends Section
case class PartialSection(parts: Part*) extends Section

case class Unification(unifies: Name*) extends Syntax // foo = bar = baz
case class Rule(forAll: List[Name], equiv: Exp*) extends Syntax // f g -> f . rmap g = lmap f . g

sealed trait Part extends Syntax
case class PartMutual(mutual: List[QName]*) extends Part             // map join <-> bind 
case class PartImpl(from: List[QName], to: List[QName]) extends Part // foo -> bind
