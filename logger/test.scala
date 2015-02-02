object Test extends App {
  val avalue = "AValue"
  def f(v: String) = "`v`";


  LoggerMacro.log("My first log");

  // LoggerMacro.log("My second log" + avalue + " test" + avalue);

  // Apply(
  //   Select(
  //     Apply(
  //       Select(
  //         Apply(
  //           Select(
  //             Literal(Constant("My second log")), TermName("$plus")
  //           ),
  //           List(Select(This(TypeName("Test")), TermName("avalue")))
  //         ),
  //         TermName("$plus")
  //       ),
  //       List(Literal(Constant(" test")))
  //     ),
  //     TermName("$plus")
  //   ),
  //   List(Select(This(TypeName("Test")), TermName("avalue")))
  // )

  // LoggerMacro.log("My second log" + avalue + " test" + f(avalue));

  // Apply(
  //   Select(
  //     Apply(
  //       Select(
  //         Apply(
  //           Select(
  //             Literal(Constant("My second log")), TermName("$plus")
  //           ),
  //           List(Select(This(TypeName("Test")), TermName("avalue")))
  //         ),
  //         TermName("$plus")
  //       ),
  //       List(Literal(Constant(" test")))
  //     ),
  //     TermName("$plus")
  //   ),
  //   List(
  //     Apply(
  //       Select(
  //         This(TypeName("Test")), TermName("f")
  //       ),
  //       List(
  //         Select(
  //           This(TypeName("Test")), TermName("avalue")
  //         )
  //       )
  //     )
  //   )
  // )

  LoggerMacro.log(avalue + "My second log test" + f(avalue));

  // Apply(
  //   Select(
  //     Apply(
  //       Select(
  //         Select(
  //           This(TypeName("Test")), TermName("avalue")
  //         ),
  //         TermName("$plus")
  //       ),
  //       List(
  //         Literal(
  //           Constant("My second log test")
  //         )
  //       )
  //     ),
  //     TermName("$plus")
  //   ),
  //   List(
  //     Apply(
  //       Select(
  //         This(TypeName("Test")),
  //         TermName("f")
  //       ),
  //       List(
  //         Select(
  //           This(TypeName("Test")),
  //           TermName("avalue"))
  //       )
  //     )
  //   )
  // )

  LoggerMacro.log(s"My second log $avalue");
}
