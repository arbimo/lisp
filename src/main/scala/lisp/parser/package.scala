package lisp

package object parser {

  /** An S-Expression */
  type E = AnyRef

  def parse(str: String): E = fastparse.parse(str, Parser.parser(_))
}
