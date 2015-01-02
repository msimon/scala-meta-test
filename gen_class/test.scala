// object Test {
//   import Macros._

//   val _ = test

// }


// object Other extends App {
//   println("toto is :" + Test.method)
// }

Macros.test

object Lol extends App {

  // val instance = Macros.test

  val t = Macros.test

  def apply() = {
    t
  }
  // val m = method
  // val toto = toto

  // println("m : " + m + " toto: " + toto);
}

object Toto extends App {
  val t = Lol()

  val m = t.method
  val toto = t.toto

  println("m : " + m + " toto: " + toto);
}
