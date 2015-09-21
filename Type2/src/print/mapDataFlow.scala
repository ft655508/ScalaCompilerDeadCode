/*
 * Calculate Type 2 dead code ratio for each compiler source file. 
 */

import scala.io.Source
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import java.io._
import scala.Predef
import collection.mutable.ArrayBuffer
import collection.mutable.Queue

class mapDataFlow {
  
  val dataFlowMap : Map[String, String] = Map[String, String]()
  val fileMap : Map[String, Int] = Map[String, Int]()
  val useMap : Map[String, Int] = Map[String, Int]()
  val outputFile = new java.io.FileWriter("partialresult", true)
  
  def readDataFlow() {
    val iterator = Source.fromFile("pureHelloDataFlow").getLines()
    while(iterator.hasNext) {
     val line = iterator.next
     if(line.contains(">>>")) {
     val file = iterator.next
     if(!dataFlowMap.contains(line))
       dataFlowMap += ((line, file))
     if(!fileMap.contains(file))
       fileMap += ((file, 1))
     else 
       fileMap(file) = fileMap(file) + 1
     }
    }
  }
  
  def readUseFlow() {
    val iterator = Source.fromFile("oldResult").getLines()
    val lineset = Set[String]()
    for(line <- iterator)
      if(!lineset.contains(line)) {
      if(dataFlowMap.contains(line))
      if(useMap.contains(dataFlowMap(line)))
        useMap(dataFlowMap(line)) = useMap(dataFlowMap(line)) + 1
      else
        useMap += ((dataFlowMap(line), 1))
        lineset += line
      }
  }
  
  def printResult() {
    for(file <- fileMap.keySet)  
      if(useMap.contains(file))
        outputFile.write(file + " " + (fileMap(file) - useMap(file)) + "\\" + fileMap(file) + "\n")
      else 
        outputFile.write(file + " " + fileMap(file) + "\\" + fileMap(file) + "\n")
  }
}

object mapDataFlow {
  
  val datamap = new mapDataFlow()
  
  def main(args : Array[String]) {
    datamap.readDataFlow()
    datamap.readUseFlow()
    datamap.printResult()
    datamap.outputFile.flush()
    datamap.outputFile.close()
  }
}