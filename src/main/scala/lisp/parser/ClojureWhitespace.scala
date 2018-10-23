package lisp.parser

import fastparse.ParsingRun

import scala.annotation.{switch, tailrec}

object ClojureWhitespace {
  implicit val whitespace = {implicit ctx: ParsingRun[_] =>
    val input = ctx.input
    @tailrec def rec(current: Int, state: Int): ParsingRun[Unit] = {
      if (!input.isReachable(current)) ctx.freshSuccess((), current)
      else {
        val currentChar = input(current)
        (state: @switch) match{
          case 0 =>
            (currentChar: @switch) match{
              case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state)
              case ';' => rec(current + 1, state = 1)
              case _ => ctx.freshSuccess((), current)
            }
          case 1 => rec(current + 1, state = if (currentChar == '\n') 0 else state)
        }
      }
    }
    rec(current = ctx.index, state = 0)
  }
}
