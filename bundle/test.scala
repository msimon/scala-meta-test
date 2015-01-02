object Macros {
  def mono = macro Impl.mono
  def poly[T] = macro Impl.poly[T]
}

object Test extends App {
  Macros.mono;
  Macros.poly[Int];
}
