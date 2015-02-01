object Test extends App {
  val u = Macros.poly[String];
  println("u:" + u)

  Macros.mono;
  val i = Macros.poly[Int];
  println("i:" + i)

  val j = Macros.arg(10);
  println("j:" + j)
}
