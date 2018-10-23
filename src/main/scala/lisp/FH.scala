package lisp

import java.lang.invoke.{CallSite, MethodHandle, MethodHandles, MethodType}

sealed trait FH {
  def handle: MethodHandle
}

class FH0[+R](val handle: MethodHandle) {
  def withDummyArg[A: Class]: FH1[A, R] =
    new FH1(MethodHandles.dropArguments(handle, 0, implicitly[Class[A]]))
}

class FH1[-A, +R](val handle: MethodHandle) {

  def bindTo(a: A): FH0[R] = new FH0[R](handle.bindTo(a))
  def apply(a: A): R = handle.invoke(a)
}

class FH2[-A, -B, +R](val handle: MethodHandle) {

  def bindTo(a: A): FH1[B, R] = new FH1(handle.bindTo(a))
}


object FH {

  val publicLookup: MethodHandles.Lookup = MethodHandles.publicLookup()


  val flatInvoke: MethodHandle = publicLookup.findStatic(classOf[mh.Fun], "flattenInvoke", MethodType.methodType(classOf[mh.Fun], classOf[MethodHandle], classOf[Object], classOf[Object]))
  val fi: FH2[MethodHandle, Any, Nothing]= new FH2(flatInvoke)
  def yy[A, B](f: A => (() => B)): A => B = a => f(a).apply()

  def xx[A, B](f: FH1[A, FH0[B]]): FH1[A, B] = {
    val h = f.handle
    fi.bindTo(h)
  }

//  val fromEnv:

  def reif[A, B](f: A => B): FH1[A, B] = ???

//    f.handle.
}

object XXX extends App {

  def getSym(a: Sym, e: EnvComp): FH0[AnyRef] = e.getOrCreateLocalHandle(a)
  FH.reif[Int, Int](identity)

}