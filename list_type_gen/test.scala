import reflect.macros.blackbox._
import language.experimental.macros
import scala.reflect.runtime.universe._

object Testo {
  def apply(l: List[String]) : Unit = macro impl
  def impl(c: Context)(@throws l: c.Tree) = {
    import c.universe._

    // val q"..$stats" = l;

    println(showCode(l));

    q""
  }
}




Testo(List[String] (
  "Test1",
  "Test2"
))



class Reader(fname: String) {
  private val in = new BufferedReader(new FileReader(fname))
  @throws(classOf[IOException])
  def read() = in.read()
}


import scala.language.experimental.macros
import reflect.macros.Context



trait ToStringAdder {
  def add(param: Any): Unit = macro ToStringAdder.toStringAndValueImpl
  def add(param: Any, toStringBasedOnAST: String): Unit ; //This is the actual method I want the above method call to be replaced by
}

object ToStringAdder {
  def toStringAndValueImpl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    val paramRep = show(param.tree)
    reify { (c.Expr[ToStringAdder](c.prefix.tree)).splice.add(param.splice, c.literal(paramRep).splice) }
  }
}


class ToStringAdder1 extends ToStringAdder {
  def add(param: Any, toStringBasedOnAST: String): Unit = println("param: " + params + "  & toStringBasedOnAST: " + toStringBasedOnAST)
}

new ToStringAdder1().add( (i: Int) => i*2)
