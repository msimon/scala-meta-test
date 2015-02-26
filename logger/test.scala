object Test extends App with Logging {
  object T2 {
    def f2 (v : String) = 10
    def f(v: String) = f2(v);
  }
  val avalue = "AValue"
  val bvalue = "BValue"
  def f(v: String) = "`v`";

  // with +
  // logger.info("low_test", Logger.Low)
  // logger.info("test")

  // logger.info("This should work=" + avalue);
  // logger.info("This should work=" + avalue + " test=" + bvalue);
  // logger.info("This should work=" + avalue + " test=" + T2.f(avalue) + " ENd");
  // logger.info("This should work with whitespace= " + avalue + " test= " + T2.f(avalue) + " ENd");

  // Logger.info(avalue + "Hello")
  // Logger.info("My third log " + 10 + " hello " + 12);
  // Logger.info("My third log " + 10 + " hello " + 12 + " dsadasds");
  // Logger.info("Second term illegal=" + avalue + " test" + T2.f(avalue) + "dasdasd ads");
  // Logger.info("Firt term illegal with whitspace   " + avalue + " test=" + T2.f(avalue) + "dasdasd ads");



  //with s
  // logger.info(s"test")
  // logger.info(s"""This should work=$avalue test=$bvalue""");
  // logger.info(s"""This should work=$avalue test=${T2.f(avalue)} ENd""");
  // logger.info(s"""This should work with whitespace= $avalue test= ${T2.f(avalue)} ENd""");

  logger.info(s"[groupId=$bvalue] Upload scoring csv files \nto S3 bucket=$avalue...\n statsSt\nr=$avalue\n")

  // Logger.info(s"""$avalue Hello""")
  // Logger.info(s"""Second term illegal=$avalue test ${T2.f(avalue)} dasdasd ads""");
  // Logger.info(s"""Firt term illegal with whitspace    $avalue test=${T2.f(avalue)} dasdasd ads""");
  // Logger.info(s"""First term illegal with whitspace $avalue test=${T2.f(avalue)}""")
}
