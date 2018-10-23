import java.lang.invoke._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

package object lisp {


  def error(msg: String): Nothing = throw Err(msg)
  def unsupported(msg: String): Nothing = error(s"Unsupported: $msg")

  object keywords {
    final val QUOTE = Sym("quote")
    final val IF = Sym("if")
    final val DEFINE = Sym("define")
    final val ATOM = Sym("atom")
    final val CONS = Sym("cons")
    final val LAMBDA = Sym("lambda")
    final val DO = Sym("do")
  }
  import keywords._

  type SExpr = Any

  final case class Sym(name: String) {
    override def toString: String = name
  }

  def parse(str: String): SExpr = {
    parser.parse(str) match {
      case fastparse.Parsed.Success(sexpr, _) => sexpr
      case x: fastparse.Parsed.Failure => error(x.toString())
    }
  }

  def compile(e: SExpr): FH1[EnvComp, AnyRef] = {
    e match {
//      case x: Sym => ??? //env.getCallSite(x).dynamicInvoker()
//      case x: Int => MethodHandles.constant(classOf[Int], x)
//      case IF :: cond :: onTrue :: onFalse :: Nil =>
//        val condMH = compile(cond, env).asType(MethodType.methodType(classOf[Boolean]))
//        //        println(condMH.asType(MethodType.methodType(classOf[Boolean])))
//        //        println(condMH.invoke())
//        val onTrueMH = compile(onTrue, env)
//        val onFalseMH = compile(onFalse, env)
//        MethodHandles.guardWithTest(condMH, onTrueMH, onFalseMH)
//      //        match {
//      //          case true => eval(onTrue, env)
//      //          case false => eval(onFalse, env)
//      //          case x => error("Unexpected condition result: $x")
//      //        }
//      case QUOTE :: tail => compile(tail, env)
//      //      case EQ :: tail => tail.map(eval(_, env)) match {
//      //        case a :: b :: Nil => a == b
//      //      }
//      case DEFINE :: Sym(label) :: expr =>
//        val mh = compile(expr, env)
//        env.setHandle(label, mh)
//        mh
//      case LAMBDA :: args :: body :: Nil =>
//        args match {
//          case l: Seq[_] =>
//            val args: Array[Sym] = l.map {
//              case x: Sym => x
//              case x => error(s"Lambda param is not a symbol but $x")
//            }.toArray
//            val envCreate: FH1[Array[AnyRef], Env] = env.subEnvHandle.bindTo(args)
//        }
//
//      case l: List[Any] => l.map(compile(_, env)) match {
//        case f :: args =>  //f.invokeWithArguments(args: _*)
//          f.invoke() match {
//            case mh: MethodHandle => MethodHandles.constant(classOf[AnyRef], mh.invokeWithArguments(args.map(_.invoke()): _*))
//            case x => error(s"Expected function (method handle) but got $x")
//          }
//      }
      case Nil => ???
    }
  }


  def eval(e: SExpr, env: Env): Any = {
    e match {
      case x: Sym => env.getValue(x) //env.getCallSite(x).dynamicInvoker().invoke()
      case x: Int => x
      case x: String => x
      case DO :: rest =>
        rest.foreach(eval(_, env))
      case IF :: cond :: onTrue :: onFalse :: Nil =>
        eval(cond, env) match {
          case true => eval(onTrue, env)
          case false => eval(onFalse, env)
          case x => error(s"Unexpected condition result: $x")
        }
      case QUOTE :: e :: Nil => e
      case ATOM :: (_: List[_]) :: Nil => false
      case ATOM :: _ :: Nil => true
      case LAMBDA :: (args: Seq[Sym]) :: exp :: Nil =>
        mh.mh((params: Array[AnyRef]) => {
          eval(exp, env.subEnv(args.toArray, params))
        })
          .asCollector(classOf[Array[AnyRef]], args.size)
      case DEFINE :: Sym(label) :: expr :: Nil =>
        val mh = eval(expr, env)
        env.setConstantValue(label, mh)
        mh
      case Nil => false
      case l: List[AnyRef] =>
        l.head match {
          case _: List[_] => unsupported(s"nested list $l")
          case LAMBDA | DEFINE | QUOTE | IF => error(s"Malformed expression $l")
          case _ =>
        }
        val X = l.map(eval(_, env))
        X match {
        case (f: MethodHandle) :: (args: List[AnyRef]) =>
          f.invokeWithArguments(args: _*)
        case x =>
          error(s"Expected function (method handle) but got $x")
      }

    }
  }

  def parseEval(str: String, env: Env): Any = {
    eval(parse(str), env)
  }

  def format(e: Any): String = e match {
    case l: List[_] => l.map(format).mkString("(", " ", ")")
    case e => e.toString
  }
}

package lisp {
  case class Err(msg: String) extends Exception(msg) {

  }

  object Test extends App {

    val env = new Env

    env.setConstantValue("true", true)
    env.setConstantValue("false", false)
    env.setFunctionHandle("+", mh.Fun.getIII("iadd"))
    env.setFunctionHandle("*", mh.Fun.getIII("imul"))
    env.setFunctionHandle("<", mh.mh((a: Int, b: Int) => a < b))
    env.setFunctionHandle(">", mh.mh((a: Int, b: Int) => a > b))
    env.setFunctionHandle("<=", mh.mh((a: Int, b: Int) => a <= b))
    env.setFunctionHandle("=", mh.mh((a: AnyRef, b: AnyRef) => a == b))
    env.setFunctionHandle("and", mh.mh((a: Boolean, b: Boolean) => a && b))
    env.setFunctionHandle("or", mh.mh((a: Boolean, b: Boolean) => a || b))
    env.setFunctionHandle("first", mh.mh((a: List[AnyRef]) => a.head))
    env.setFunctionHandle("rest", mh.mh((a: List[AnyRef]) => a.tail))
    env.setFunctionHandle("cons", mh.mh((a: AnyRef, l: List[AnyRef]) => a ::l))
    env.setFunctionHandle("print", mh.mhVarArgs((a: Array[AnyRef]) => {
      println(a.map(format).mkString(" ")); a
    }))
    env.setFunctionHandle("printerr", mh.mhVarArgs((a: Array[AnyRef]) => {
      System.err.println(a.map(format).mkString(" ")); a
    }))

    env.setFunctionHandle("map", mh.mh( (f: MethodHandle, l: List[AnyRef]) => l.map(a => f.invoke(a))))
    env.setFunctionHandle("foldl", mh.mh( (f: MethodHandle, acc: AnyRef, l: List[AnyRef]) =>
      l.foldLeft(acc)((acc, a) => f.invoke(acc, a))))

    parseEval(
      """
        (do
          (defn empty [l] (if (= l '()) true false))
          (defn min [a b] (if (< a b) a b))
          (defn reduce [f l] (foldl f (first l) (rest l)))
          (defn sum [l] (foldl + 0 l))
          (defn forall [f l] (foldl and true (map f l)))
          (defn exists [f l] (foldl or false (map f l)))
          (defn check-eq [a b] (if (= a b) true (printerr "Not equal" a b)))
          ;(defn map [f l]  (cons (f (first l))
          ;    (if (empty (rest l)) '() (map f (rest l)))))
          (defn inc [a] (+ a 1))
          (check-eq (map inc '(1 2)) '(2 3))
          (check-eq (sum '(1 2 3 4)) 10)
          (check-eq (reduce + '(1 2 3)) 6)
          (define l '(1 2 3))
          (check-eq (forall (fn [x] (> x 1)) l) false)
          (check-eq (forall (fn [x] (> x 0)) l) true)
          (check-eq (exists (fn [x] (> x 0)) l) true)
          (check-eq (exists (fn [x] (> x 4)) l) false)
          (check-eq (exists (fn [x] (> x 2)) l) true)
          (defn is-big [n] (> n 10))
          (check-eq (exists is-big l) false)
        )
      """.stripMargin, env)

  }

}