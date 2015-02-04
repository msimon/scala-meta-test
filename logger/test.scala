object Test extends App {
  object T2 {
    def f2 (v : String) = 10
    def f(v: String) = f2(v);
  }
  val avalue = "AValue"
  val bvalue = "BValue"
  def f(v: String) = "`v`";

  // // with +
  // LoggerMacro.log("test")
  // LoggerMacro.log("This should work=" + avalue + " test=" + bvalue);
  // LoggerMacro.log("This should work=" + avalue + " test=" + T2.f(avalue) + " ENd");
  // LoggerMacro.log("This should work with whitespace= " + avalue + " test= " + T2.f(avalue) + " ENd");

  // LoggerMacro.log("My third log " + 10 + " hello " + 12);
  // LoggerMacro.log("My third log " + 10 + " hello " + 12 + " dsadasds");
  // LoggerMacro.log(avalue + "Hello")
  // LoggerMacro.log("Second term illegal=" + avalue + " test" + T2.f(avalue) + "dasdasd ads");
  // LoggerMacro.log("Firt term illegal with whitspace   " + avalue + " test=" + T2.f(avalue) + "dasdasd ads");



  // with s
  LoggerMacro.log(s"test")
  LoggerMacro.log(s"""This should work=$avalue test=$bvalue""");
  LoggerMacro.log(s"""This should work=$avalue test=${T2.f(avalue)} ENd""");
  LoggerMacro.log(s"""This should work with whitespace= $avalue test= ${T2.f(avalue)} ENd""");

  // LoggerMacro.log(s"""$avalue Hello""")
  // LoggerMacro.log(s"""Second term illegal=$avalue test ${T2.f(avalue)} dasdasd ads""");
  // LoggerMacro.log(s"""Firt term illegal with whitspace    $avalue test=${T2.f(avalue)} dasdasd ads""");
  // LoggerMacro.log(s"""First term illegal with whitspace $avalue test=${T2.f(avalue)}""")
}
