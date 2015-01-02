import reflect.macros.blackbox._
import language.experimental.macros
import scala.reflect.runtime.universe._

object debug {
  def apply[T](x: =>T): T = macro impl
  def impl(c: Context)(x: c.Tree) = { import c.universe._
    val q"..$stats" = x
    val loggedStats = stats.flatMap { stat =>
      val msg = "executing " + showCode(stat)
      List(q"println($msg)", stat)
    }
    q"..$loggedStats"
  }
}
// usage
object Test extends App {
  def faulty: Int = throw new Exception

  debug {
    val x = 1
    val y = x + faulty
    x + 5
  }
}


Test.main(Array())

10 + 5

object Macro {
  def apply(x: Int): Int = macro impl
  def impl(c: Context)(x: c.Expr[Int]): c.Expr[Int] = { import c.universe._
    c.Expr(q"$x + 1")
  }
}


object Macro {
  def apply(x: Int): Int = macro impl
  def impl(c: Context)(x: c.Tree) = { import c.universe._
    q"$x + 1"
  }
}

Macro(5)



val name = TypeName("Foo")
val foo = tq"$name"
val tq"${name: TypeName}" = tq"Foo"


val patdef = q"val (x, y) = (1, 2)"
val tupsum = q"..$patdef; a + b"
val wrongtupsum = q"$patdef; a + b"

q"{ val c = a + b; b + c}; a + b"


trait Base
trait A extends Base
trait B extends Base
trait C extends B

class Alpha {
  type T >: B <: Base
}

class Beta extends Alpha {
  type T = C
}

val q"class Alpha extends ..$e { $t }" = q"class Alpha extends Bol with Toto { type T >: B <: Base }"

val q"$mods type $name[..$tparams] >: $low <: $high" = t


val q"$mods type $name[..$args] = $tpt" = q"type Foo[T] = List[T]"
val q"$mods type $name[..$args] = $tpt" = q"type Foo[T] >: Foo[Bar] <: List[T]"

val q"$mods type $name[..$args] = $tpt" = q"type Foo[T] <: List[T]"
val TypeBoundsTree(low, high) = tpt

val q"$mods def $name[..$tparams](...$paramss): $tpt = $body" = q"def f = { val a = 3; val b = a + 4; val c = a + b; c }"
val q"def g(...$paramss) = $body" = q"def g(x: Int)(implicit y: Int, v: Int) = x + y"

val q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =
  q"""class A[T] (val b : Int, val c: String) extends { val name = "class Y" } with B with C with E { val a = 1 }"""

val P = q"package object P"


// do not work in REPL
import scala.reflect.macros.blackbox.Context

class Impl(val c: scala.reflect.macros.blackbox.Context) {
  def mono = c.literalUnit
  def poly[T: c.WeakTypeTag] = c.literal(c.weakTypeOf[T].toString)
}

object Macros {
  def mono = macro Impl.mono
  def poly[T] = macro Impl.poly[T]
}


// Implicite Macro: Type macro
trait Showable[T] { def show(x: T): String }
def show[T](x: T)(implicit s: Showable[T]) = s.show(x)

// implicit object IntShowable extends Showable[Int] {
//   def show(x: Int) = x.toString
// }

object ShowableInt extends Showable[Int] { def show (x:Int) = x.toString }

implicit object Int2Showable extends ShowableInt;

implicit object StringShowable extends Showable[String] {
  def show(x: String) = x
}

implicit object String2Showable extends Showable[String] {
  def show(x: String) = x
}

show(42) // "42"
show("toto")
