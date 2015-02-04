import language.experimental.macros
import reflect.macros.whitebox._

object LoggerMacro {
  val regexpSplit = "[^\\s]+".r

  def log(x : String) : Unit = macro logImpl

  def logImpl(c: Context)(x: c.Tree) : c.Tree = {
    import c.universe._

    def pseudoChecker(t: c.Tree, a: String = "", root: Boolean = true) : (String, c.Tree) = {
      def checkLastChar(str: String, value: Tree) = {
        str.trim.lastOption match {
          case Some(ch) if ch == '=' => ()
          case _ => c.error(c.enclosingPosition, s"""Term `$value` is not associated to a key. Check bit.ly/18MsDt9 for documentation""")
        }
      }

      def concatTermName(name: String) = {
        checkLastChar(a, t);
        (a + name, t)
      }

      def capitalizeKey(keyStr: String) = {
        val l = regexpSplit.findAllIn(keyStr).toList;

        l.lastOption match {
          case Some(v) => l.dropRight(1).mkString(" ") + " " + v.toUpperCase + "'";
          case None => throw new Exception // should never happen (the illegal structure  will be raised)
        }
      }

      t match {
        case q"""scala.StringContext.apply(..$lcons).s(..$lparams)""" => { // use of s""
          def concat(lcons : List[Tree], lpar: List[Tree], acc: String = "", newLcons: List[String] = List()) : (String,List[String]) = {
            (lcons.headOption, lpar.headOption) match {
              case (Some(Literal(Constant(cons: String))), Some(par)) => {
                checkLastChar(cons, par);
                concat(lcons.tail, lpar.tail, acc + cons + par, capitalizeKey(cons)::newLcons)
              }
              case (Some(Literal(Constant(cons: String))), None) => (acc + cons, ("'" + cons)::newLcons)
              case _ => ("", List[String]())
            }
          }

          val (s, newLcons) = concat(lcons, lparams)
          val newTree = q"""scala.StringContext.apply(..${newLcons.reverse}).s(..$lparams)""";

          (s, newTree)
        }

        case Literal(Constant(b: String)) => {
          val ret = a + b;

          // if it's the first iteration, we do not capitalize the last word of string (only place where it's not a KEY)
          if (root == true) {
            (ret,t)
          } else {
            (ret, Literal(Constant(capitalizeKey(b))))
          }
        }

        case Literal(Constant(b)) => {
          c.error(c.enclosingPosition, "Constant other than String are not accepted in log")
          ("", t)
        }

        case Apply(Select(selectBody, TermName("$plus")), (applyParam::Nil)) => { // use of '+'
          val (b,newSelectBody) = pseudoChecker(selectBody, a, false);
          val (ret,newApplyParam) = pseudoChecker(applyParam, b, root);

          (ret, Apply(Select(newSelectBody, TermName("$plus")), List(newApplyParam)))
        }

        case Apply(Select(_, TermName(name)), _) => { // function application
          concatTermName(name)
        }

        case Select(_, TermName(name)) => { // value
          concatTermName(name)
        }
      }
    }

    val (_,tree) = pseudoChecker(x);

    q"""
      println($tree)
    """
  }
}
