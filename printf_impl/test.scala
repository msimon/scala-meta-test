object Test extends App {

  val lol = "abc";
  {
    val eval$macro$1: String = Test.this.lol;
    Predef.print("hello ");
    Predef.print(eval$macro$1);
    Predef.print("!");
    ()
  }

}

Test.main(Array())
