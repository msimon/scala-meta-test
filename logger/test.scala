object Test extends App {
  object T2 {
    def f(v: String) = "`v`";
  }
  val avalue = "AValue"
  val bvalue = "BValue"
  def f(v: String) = "`v`";

  LoggerMacro.log("My first log");
  LoggerMacro.log("My second log=" + avalue);

  LoggerMacro.log("My second log=" + avalue + " test" + avalue);

  // LoggerMacro.log("My second log" + avalue + " test" + f(avalue));

  // LoggerMacro.log(avalue + "My second log test=" + T2.f(avalue));

  LoggerMacro.log(s"My second log=$avalue, $bvalue");
}
