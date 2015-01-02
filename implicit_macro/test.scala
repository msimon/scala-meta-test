import scala.language.experimental.macros

object Test extends App {
  def show[T](x: T)(implicit s: Showable[T]) = s.show(x)

  class C(val x: Int) {
    val l = List(1, 3, 4)
    val l2 = List(List("1", "2"), List("3", "4"))
    val l3 = List(12.3, 15.4)
    // var y : Int = 10;
    // private val z : Int = 15;
  }
  class D(val x: Int) {
    // val c : C = new C(12);
    // val y : Int = 11;
    // val t : String = "Str";
    // val l : List[Int] = List(10,23,55);
  }

  val c = new C(10);
  println(show(c));
  // println(show(new D(3)))
}
