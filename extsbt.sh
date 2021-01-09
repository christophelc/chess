if [ -z "$GRAALVM_HOME" ]; then 

	echo "Using Standard JVM";
	sbt
else 
	echo "Using Graal VM";
	sbt -java-home $GRAALVM_HOME
fi
#sbt -java-home $GRAALVM_HOME
