#!/bin/bash

echo "Start of scalac"
cd classes
scalac ../OutputLineFile.scala
echo "End of scalac"
jar cf ../output.jar .
cd ..
echo "Plugin Begins"
scalac -Xplugin:output.jar hello.scala counter.scala
echo "End of Plugin"

exit 0
