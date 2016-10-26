

INCLUDE_PATH=lib/antlr-4.5.3-complete.jar

all:
	mkdir -p out
	scalac -cp ${INCLUDE_PATH} -d out src/*/*.scala src/*/*.java

	javac -d out \
      -cp ${INCLUDE_PATH}:lib/scala-library.jar:out \
       src/*/*.java
