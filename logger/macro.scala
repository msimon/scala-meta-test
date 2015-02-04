import language.experimental.macros
import reflect.macros.whitebox._

object LoggerMacro {
  def log(x : String) : Unit = macro logImpl
  def logImpl(c: Context)(x: c.Tree) : c.Tree = {
    import c.universe._

    val argDeliStart = "|>"
    val argDeliEnd = "<|"
    def treeToString(t: c.Tree, a: String = "") : String = {
      t match {
        case Literal(Constant(b: String)) => a + b

        case q"""scala.StringContext.apply(..$lcons).s(..$lparams)""" => // use of s""

          def concat(lcons : List[Tree], lpar: List[Tree], acc: String= "") : String = {
            (lcons.headOption, lpar.headOption) match {
              // case (Some(Literal(Constant(cons))), Some(Ident(TermName(par)))) => concat(lcons.tail, lpar.tail, acc + cons + "$>" + par)
              case (Some(Literal(Constant(cons))), Some(par)) => concat(lcons.tail, lpar.tail, acc + cons + argDeliStart + par + argDeliEnd)
              case (Some(Literal(Constant(cons))), None) => acc + cons
              case _ => ""
            }
          }
          concat(lcons, lparams)

        case Apply(Select(selectBody, TermName("$plus")), (applyParam::Nil)) => // use of '+'
          val b = treeToString(selectBody, a);
          treeToString(applyParam, b);

        case Apply(Select(_, TermName(name)), _) => // function application
          val b = argDeliStart + name + argDeliEnd
          a + b
        case Select(_, TermName(name)) => // value
          val b = argDeliStart + name + argDeliEnd
          a + b
      }
    }

    def pseudoChecker(s: String) = {
      val m = java.util.regex.Pattern.compile(
        """((\w+)?\=?)\s*""" + argDeliStart.replace("|", "\\|") + """(.*?)""" + argDeliEnd.replace("|", "\\|")
      )
      val res = m.matcher(s);

      while (res.find) {
        val key = res.group(1);
        val value = res.group(3);
        println(s"""key: "$key" value: "$value"""");
        key.lastOption match {
          case (Some(ch)) if (ch == '=') => ()
          case _ => c.error(c.enclosingPosition, "Ill Structured Log\n")
        }
        println("> Pass\n");
      }
    }

    val strToParse = treeToString(x);
    pseudoChecker(strToParse);

    q"""
       println("Loggin: " + $x)
     """
  }
}
