import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

class Impl(val c: Context) {
  import c.universe._
  def mono = { println("monoMacro"); Literal(Constant(())) }
  def poly[T: c.WeakTypeTag] = { println("polyMacro"); Literal(Constant(c.weakTypeOf[T].toString)) }
  def arg(x: c.Tree) : c.Tree = x
}

object Macros {
  def mono : Unit = macro Impl.mono
  def poly[T] : String = macro Impl.poly[T]
  def arg(x: Int) : Int = macro Impl.arg
}
