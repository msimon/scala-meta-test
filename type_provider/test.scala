// import scala.language.reflectiveCalls
import scala.language.experimental.macros

object Test extends App {
  import Macros._

  val db = h2db("db:tot")

  // val a /= Test.db.Tot("av", 13);

  db.add("av", 13)
  db.tots.foreach({a => println(a)})
  println("v = " + db.v)
}
