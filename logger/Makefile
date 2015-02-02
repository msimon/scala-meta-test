make:
	scalac -deprecation macro.scala && scalac -deprecation test.scala

debug:
	scalac macro.scala && scalac -Ymacro-debug-lite test.scala

clean:
	rm -f *.class

do:
	scala Test $(ON)
