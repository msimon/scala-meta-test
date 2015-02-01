do:
	scalac -feature -deprecation macros.scala && scalac -feature -deprecation test.scala && scala -feature -deprecation Test

debug:
	scalac macros.scala && scalac -Ymacro-debug-lite test.scala && scala -feature Test

clean:
	rm *.class
