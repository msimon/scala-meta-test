import language.experimental.macros
import reflect.macros.blackbox._

trait Logging {
  protected lazy val logger: Logger = new Logger
}

object Logger {
  val regexpSplit = """[^\s]+""".r

  trait Level
  case object Low extends Level
  case object Medium extends Level
  case object High extends Level
}

final class Logger {
  def debug(x: String) : Unit = macro LoggerMacro.logDefaultLevelImpl
  def info(x: String) : Unit = macro LoggerMacro.logDefaultLevelImpl

  def debug(x: String, lvl : Logger.Level) : Unit = macro LoggerMacro.logImpl
  def info(x: String, lvl : Logger.Level) : Unit = macro LoggerMacro.logImpl
  def warn(x : String, lvl: Logger.Level) : Unit = macro LoggerMacro.logImpl
  def error(x : String, lvl: Logger.Level) : Unit = macro LoggerMacro.logImpl
}

class LoggerMacro(val c: Context) {
  import c.universe._

  def logDefaultLevelImpl(x: c.Tree) : c.Tree = {
    // if we extract prefix as $prefix(..$param) we will fetch the type of prefix
    // forbiding the call to overloading function
    val q"$prefix.$fname(..$params)" = c.macroApplication
    val p = params :+ q"Logger.Low";

    q"$prefix.$fname(..$p)"
  }

  def logImpl(x: c.Tree, lvl: c.Tree) : c.Tree = {
    //    println("\n\n")
    def getConsAndParam(t: c.Tree, lcons: List[Tree] = List(), lparams: List[Tree] = List()) : (List[Tree], List[Tree]) = {
     // println(showRaw(t));
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

      def capitalizeKey(keyStr: String) : String = {
        val l = Logger.regexpSplit.findAllIn(keyStr).toList;

        l.lastOption match {
          case Some(v) => {
            // we must remove all \n \t ... from the key before we capitalize
            val vClean = v.replaceAll("""(\\.)""", "$1 ")
            if (vClean != v)
              l.dropRight(1).mkString(" ") + capitalizeKey(vClean)
            else
              l.dropRight(1).mkString(" ") + " " + v.replaceAll("""\\.""", "").toUpperCase
          }
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
              if (p.tpe != null && p.tpe =:= typeOf[String]) { (c + "'", true) }
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

    val q"$prefix.$fname(..$params)" = c.macroApplication;
    val Select(Ident(_), TermName(lvlStr: String)) = lvl;
    val tree = pseudoChecker(lcons.reverse, lparam.reverse)

    // println(showRaw(tree));
    // println(showCode(tree));


    q"""
      println("LVL=" + $lvlStr + " " + $tree)
    """
  }
}
