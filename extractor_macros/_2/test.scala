object Test extends App {
  import Interpolation._
  44 match {
    case t"$x" => println(x)
  }
}
