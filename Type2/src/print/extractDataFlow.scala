/*
 * Tool for extracting data flows from the log file, which contains both data flows and line numbers of 
 * executed codes used by Type 1. To run extractDataFlow, just specify names of the output log files.
 * 
 */

import scala.io.Source
import scala.collection.mutable.Set
import java.io._
import scala.Predef

class lineReader(fileName : String) {
  val lineTable : Set[String] = Set()
  
  var outputFile = new java.io.FileWriter(fileName.split(".txt")(0) + "Filtered.txt", true)
  
  def readAll() {
    for(line <- Source.fromFile(fileName).getLines) {
      if(!(lineTable contains line)) {
      lineTable += line
      }
    }
  }
  
  def output() {
    for(line <- lineTable) outputFile.write(line + "\n")
    outputFile.close()
  }
  
  def extract() {
    for(line <- Source.fromFile(fileName).getLines) {
      if(!line.startsWith("source-")) {
        outputFile.write(line + "\n")
      }
    }
    outputFile.close()
  }
}

object extractDataFlow {
  def main(Args : Array[String]) {
    for(i <- Args) {
      val reader = new lineReader(i)
      reader.extract()
    }
  }
}