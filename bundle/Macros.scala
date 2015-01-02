import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

class Impl(val c: Context) {
  def mono = { println("hola"); c.literalUnit }
  def poly[T: c.WeakTypeTag] = { println("holaHola"); c.literal(c.weakTypeOf[T].toString) }
}
