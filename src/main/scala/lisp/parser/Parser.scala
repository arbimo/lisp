package lisp.parser

import fastparse._
import lisp.Sym

import scala.util.Try

object Parser {

  import ClojureWhitespace._
  import lisp.keywords._

  val allowInIdents = "a-zA-Z"
  def allowed(c: Char): Boolean = c match {
    case ' ' | '\n' | '\t' | '(' | ')' | '[' | ']' => false
//    case '0' | '1' => true
    case _ => true
  }

  def parser[_: P]: P[E] = Pass ~ expr ~ End

  def atom[_: P]: P[Sym] = token.filter {
    case _: Sym => true
    case _ => false
  }.map(_.asInstanceOf[Sym])

  def expr[_: P] : P[E] = P(
    ("(" ~ "defn" ~/ atom ~/ "[" ~/ atom.rep.map(_.toList) ~/ "]" ~ expr ~ ")").map {
      case (name, params, expr) => List(DEFINE, name, List(LAMBDA, params, expr))
    }
      | ("(" ~ "fn" ~/ "[" ~/ atom.rep.map(_.toList) ~/ "]" ~ expr ~ ")").map {
      case (params, expr) => List(LAMBDA, params, expr)
    }
      | "(" ~/ expr.rep.map(_.toList) ~ ")"
      | "'" ~/ expr.map(e => QUOTE :: e :: Nil)

      | token
  )
  def token[_: P]: P[E] = P(
    "\"" ~/ CharsWhile(_ != '"').! ~ "\""
    | CharsWhile(allowed, min = 1).!.map(s => {
      Try(s.toInt) match {
        case scala.util.Success(value) => value.asInstanceOf[Integer]
        case _ => Sym(s)
      }
    })
  )
//  def number[_: P]: P[Integer] = P( CharWhiIn("0-9").rep(1).!.map(_.toInt) )
//  def atomString[_: P]: P[String] = P( CharsWhile(allowedInAtom, min = 1).! )
//  def atom[_: P]: P[Sym] = atomString
//    .map(name =>
//      Sym(name))
//  def atom[_: P]: P[Sym] = P( CharsWhileIn("a-zA-Z", 1).! ).map(Sym)
//  def parens[_: P]: P[E] = P( "(" ~/ addSub ~ ")" )
//  def factor[_: P]: P[Int] = P( number | parens )
//
//  def divMul[_: P]: P[Int] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
//  def addSub[_: P]: P[Int] = P( divMul ~ (CharIn("+\\-").! ~/ divMul).rep ).map(eval)
//  def expr[_: P]: P[Int]   = P( addSub ~ End )


}
