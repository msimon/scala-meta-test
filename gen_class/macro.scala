import scala.language.experimental.macros
import scala.reflect.macros._
import scala.collection.mutable.{ListBuffer, Stack}

object Macros {
  def test = macro testImpl

  def testImpl(c: Context): c.Expr[Any] = {
    import c.universe._
    val className = newTypeName("Testos")

    c.Expr { q"""
    class $className  {
      def method = 1
      def toto = 10
    }
  """}
  }
}
