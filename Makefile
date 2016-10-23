

INCLUDE_PATH=lib/antlr-4.5.3-complete.jar

all:
	scalac -cp ${INCLUDE_PATH} -d out src/*/*.scala src/*/*.java

	javac -d out \
      -cp ${INCLUDE_PATH} \
       src/*/*.java
