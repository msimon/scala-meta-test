import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros._

class body(tree: Any) extends StaticAnnotation

object Macros {

  def h2db (str : String) : Any = macro Macros.h2db_impl

  def selectFieldImpl(c:whitebox.Context) = {
    c.macroApplication.symbol.annotations.filter(
      _.tree.tpe <:< c.typeOf[body]
    ).head.tree.children.tail.head
  }

  def h2db_impl (c: whitebox.Context)(str: c.Tree) = {
    import c.universe._

    val Literal(Constant(s: String)) = str;

    val a = s.split(':');
    val dbName = TypeName(a(0).capitalize);
    val traitName = TypeName(a(1).capitalize);
    val valueName = TermName(a(1) + "s");

    q"""{
     trait ${dbName} {
       case class ${traitName}(val name : String, val price : Int);
       def add (name: String, price: Int) = this.${valueName}.add(new ${traitName}(name, price))
       val ${valueName}: scala.collection.mutable.Set[${traitName}] = scala.collection.mutable.Set(new ${traitName}("abc", 10))
       @body(32) def v : Int = macro Macros.selectFieldImpl
     }
     new ${dbName} {}
    }"""
  }
}
