import language.experimental.macros
import reflect.macros.blackbox._

object LoggerMacro {
  val regexpSplit = "[^\\s]+".r

  def log(x : String) : Unit = macro logImpl

  def logImpl(c: Context)(x: c.Tree) : c.Tree = {
    import c.universe._

    def getConsAndParam(t: c.Tree, lcons: List[Tree] = List(), lparams: List[Tree] = List()) : (List[Tree], List[Tree]) = {
      t match {
        case q"""scala.StringContext.apply(..$lcons).s(..$lparams)""" => (lcons.reverse, lparams.reverse) // use of s""
        case Apply(Select(selectBody, TermName("$plus")), (applyParam::Nil)) => { // use of '+'
          val (lc,lp) = getConsAndParam(selectBody, lcons, lparams)
          getConsAndParam(applyParam, lc, lp)
        }

        case Literal(Constant(_: String)) => (t::lcons, lparams)
        case Literal(Constant(_)) => {
          c.error(t.pos, "Constant other than String are not accepted in log")
          (List[Tree](), List[Tree]())
        }
        case Apply(Select(_, TermName(_)), _) | Select(_, TermName(_)) => (lcons, t::lparams)
      }
    }


    def pseudoChecker(lcons: List[Tree], lparams: List[Tree]) : c.Tree = {
      def checkLastChar(str: String, value: Tree) = {
        str.trim.lastOption match {
          case Some(ch) if ch == '=' => ()
          case _ => c.error(value.pos, s"""Term `$value` is not associated to a key. Check bit.ly/18MsDt9 for documentation""")
        }
      }

      def capitalizeKey(keyStr: String) = {
        val l = regexpSplit.findAllIn(keyStr).toList;

        l.lastOption match {
          case Some(v) => l.dropRight(1).mkString(" ") + " " + v.toUpperCase;
          case None => throw new Exception // should never happen (the illegal structure  will be raised)
        }
      }

      def concat(lcons : List[Tree], lpar: List[Tree], newLcons: List[String] = List()) : List[String] = {
        (lcons.headOption, lpar.headOption) match {
          case (Some(Literal(Constant(cons: String))), Some(par)) => {
            checkLastChar(cons, par);
            concat(lcons.tail, lpar.tail, capitalizeKey(cons)::newLcons)
          }
          case (Some(Literal(Constant(cons: String))), None) => cons::newLcons
          case _ => newLcons
        }
      }

      def addQuote(lcons: List[String], lparams: List[Tree], acc: List[String] = List[String](), prev: Boolean = false) : List[String] = {
        (lcons, lparams) match {
          case ((c::lcons),(p::lparams)) =>
            val (nc,next) = {
              if (p.tpe =:= typeOf[String]) { (c + "'", true) }
              else { (c, false) }
            }

            val nnc = {
              if (prev) { "'" + nc }
              else { nc }
            }

            addQuote(lcons, lparams, (nnc::acc), next)
          case ((c::_),_) =>
            val nc = {
              if (prev) { "'" + c }
              else { c }
            }
            (nc::acc).reverse
          case _ => acc.reverse
        }
      }

      val newLcons = concat(lcons, lparams)
      val mlcons = addQuote(newLcons.reverse, lparams);

      val newTree = q"""scala.StringContext.apply(..${mlcons}).s(..$lparams)""";

      newTree
    }

    val (lcons,lparam) = getConsAndParam(x) match {
      case (lcons, lparam) if (lcons.length == lparam.length) => (Literal(Constant(""))::lcons, lparam)
      case l => l
    }
    val tree = pseudoChecker(lcons.reverse, lparam.reverse)

    q"""
      println($tree)
    """
  }
}
