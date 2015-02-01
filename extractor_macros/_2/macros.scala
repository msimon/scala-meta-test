import scala.reflect.macros.whitebox.Context
import language.experimental.macros

object Interpolation {
  implicit class TestInterpolation(c: StringContext) {
    object t {
      def unapply(x: Int): Any = macro Macros.unapplyImpl
    }
  }
}

object Macros {
  def unapplyImpl(c: Context)(x: c.Tree) = {
    import c.universe._

    x.foreach({
      v => println(showRaw(x));
    })

    q"""
      new {
        class Match(x: Int) {
          def isEmpty = false
          def get = _1
          def _1 = x + 3
          // override def toString = "oops"
        }
        def unapply(x: Int) = new Match(x)
      }.unapply($x)
    """
  }
}


Block(List(ValDef(Modifiers(), TermName("x"), TypeTree(), Literal(Constant("3")))), Apply(Select(Select(Select(Select(Select(Ident($line11.$read), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("toInt")), List(Ident(TermName("x")))))
