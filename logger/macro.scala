import language.experimental.macros
import reflect.macros.whitebox._

object LoggerMacro {
  def log(x : String) : Unit = macro logImpl
  def logImpl(c: Context)(x: c.Tree) : c.Tree = {
    import c.universe._

    println(showRaw(x));

    q"""
       println("Loggin: " + $x)
     """
  }
}
