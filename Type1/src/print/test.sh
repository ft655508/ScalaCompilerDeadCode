#!/bin/bash

echo "Start of fsc"
fsc -d classes OutputLineFile.scala
echo "End of fsc"
cd classes
jar cf ../output.jar .
cd ..
echo "Plugin Begins"
scalac -Xplugin:output.jar hello.scala counter.scala
echo "End of Plugin"

exit 0
