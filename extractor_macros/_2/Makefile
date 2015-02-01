do:
	scalac -deprecation macros.scala && scalac test.scala -deprecation && scala Test

debug:
	scalac macros.scala && scalac -Ymacro-debug-lite test.scala && scala Test

clean:
	rm -f *.class
