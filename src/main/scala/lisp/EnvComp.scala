package lisp


import java.lang.invoke._

import scala.collection.mutable

class Env(val parent: Option[Env] = None) {
  val tpe = MethodType.methodType(classOf[AnyRef])
  type RES = FH0[AnyRef]

  private val data: mutable.Map[Sym, CallSite]= mutable.Map()

  def subEnv(sym: Sym, value: AnyRef): Env = {
    val sub = new Env(Some(this))
//    println(s"registering: $sym -> $value")
    sub.setConstantValue(sym.name, value)
    sub
  }
  def subEnv(params: Array[Sym], values: Array[AnyRef]): Env = {
    assert(params.length == values.length)
    params.zip(values).foldLeft(this) { case (e, (p, v)) => e.subEnv(p, v) }
  }


  private def getExistingCallSite(a: Sym): Option[CallSite] = {
    data.get(a).orElse(parent.flatMap(_.getExistingCallSite(a)))
  }

  def getValue(a: Sym): AnyRef =
    getExistingCallSite(a)
      .getOrElse(error(s"Unknown symbol: $a"))
      .dynamicInvoker().invoke()

  private def getOrCreateLocalCallSite(a: Sym) = {
    if(!data.contains(a)) {
      val cs = new MutableCallSite(tpe)
      data(a) = cs
    }
    data(a)
  }



  def setConstantValue(a: String, value: Any): Unit = {
    val mh = MethodHandles.constant(classOf[AnyRef], value)
    getOrCreateLocalCallSite(Sym(a)).setTarget(mh)
  }
  def setHandle(a: String, value: MethodHandle): Unit = {
    getOrCreateLocalCallSite(Sym(a)).setTarget(value)
  }
  def setFunctionHandle(a: String, value: MethodHandle): Unit = {
    println(s"Recording function $a: ${value.`type`()}")
    setConstantValue(a, value)
    //      getCallSite(Sym(a)).setTarget(value)
  }
}

class EnvComp(val parent: Option[EnvComp] = None) {
  val tpe = MethodType.methodType(classOf[AnyRef])
  type RES = FH0[AnyRef]

  private val data: mutable.Map[Sym, CallSite] = mutable.Map()

  def subEnv(sym: Sym, value: AnyRef): EnvComp = {
    val sub = new EnvComp(Some(this))
    sub.setConstantValue(sym.name, value)
    sub
  }
  def subEnv(params: Array[Sym], values: Array[AnyRef]): EnvComp = {
    assert(params.length == values.length)
    params.zip(values).foldLeft(this) { case (e, (p, v)) => e.subEnv(p, v) }
  }
  def subEnvHandle: FH2[Array[Sym], Array[AnyRef], EnvComp] = {
    val tpe = MethodType.methodType(classOf[EnvComp], classOf[EnvComp], classOf[EnvComp], classOf[Array[Sym]], classOf[Array[AnyRef]])

    new FH2(MethodHandles.publicLookup().findVirtual(classOf[EnvComp], "subEnv", tpe)
      .bindTo(this))
  }

  def getExistingCallSite(a: Sym): Option[CallSite] = {
    data.get(a).orElse(parent.flatMap(_.getExistingCallSite(a)))
  }

  def getExistingHandle(a: Sym): Option[RES] = {
    data.get(a).orElse(parent.flatMap(_.getExistingCallSite(a))).map(_.dynamicInvoker())
      .map(new FH0(_))
  }
  //    def getOrCreateCallSize()

  def getOrCreateLocalCallSite(a: Sym) = {
    if(!data.contains(a)) {
      val cs = new MutableCallSite(tpe)
      data(a) = cs
    }
    data(a)
  }

  def getOrCreateLocalHandle(a: Sym): RES = {
    new FH0(getOrCreateLocalCallSite(a).dynamicInvoker())
  }

  //    def update(a: Sym, value: Any): Unit = data(a) = value

  def setConstantValue(a: String, value: Any): Unit = {
    val mh = MethodHandles.constant(classOf[AnyRef], value)
    getOrCreateLocalCallSite(Sym(a)).setTarget(mh)
  }
  def setHandle(a: String, value: MethodHandle): Unit = {
    getOrCreateLocalCallSite(Sym(a)).setTarget(value)
  }
  def setFunctionHandle(a: String, value: MethodHandle): Unit = {
//    println(s"Recording function $a: ${value.`type`()}")
    setConstantValue(a, value)
    //      getCallSite(Sym(a)).setTarget(value)
  }
}