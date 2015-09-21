/*
 * Tool for searching the data flow file to find out the useless fragments. 
 * To run Searcher, two arguments are needed. The first one is the name of the 
 * data flow file. The second one is the starting node for the search, which is 
 * the name of an AST node in data flow file. It outputs the data flows which are 
 * touched during the search, these are useful data flows in the program.
 */

import scala.io.Source
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import java.io._
import scala.Predef
import collection.mutable.ArrayBuffer
import collection.mutable.Queue

class Searcher(dataFlow : String, startFrom : String) {
  val lineTable : ArrayBuffer[String] = ArrayBuffer[String]()
  val outputFile = new java.io.FileWriter(dataFlow + "Result", true)
  val searchQueue : Map[String, Int] = Map[String, Int]()
  val removeQueue : Set[String] = Set[String]()
  val bufferQueue : Queue[Array[String]] = Queue[Array[String]]()
  val startList : ArrayBuffer[String] = ArrayBuffer[String]()
  var readed = 0
  
  def readAll() {
    for(line <- Source.fromFile("Entry").getLines) {
      startList += (line.trim.split(" ")(0) + line.trim.split(" ")(1))
    }
    for(line <- Source.fromFile(dataFlow).getLines) {
      lineTable += line
      readed = readed + 1
      if(readed % 1000000 == 0)
        println(readed)
    }
    println("read finish")
  }
  
  def search() {
    val length = lineTable.length - 1
    var touched : Boolean = true
    var start = -1
    var startline = 0
    while(start < 0) {      
      val tempList = lineTable(length - startline).trim.split(" ")
      for(i <- 0 to tempList.length - 1) tempList(i) = tempList(i).trim
      if(tempList.contains(">>>")) {
      for(i <- tempList.indexOf(">>>") + 1 to tempList.length - 1) 
        if(tempList(i) == startFrom)
          start = startline
      }
      startline = startline + 1
      println("searching startline " + startline)
    }
    println("search startline finish")
    println(start)
    for(pos <- start to length) {
      val tempList = lineTable(length - pos).trim.split(" ")
      for(i <- 0 to tempList.length - 1) tempList(i) = tempList(i).trim
      if(tempList.contains(">>>")) {
      for(i <- tempList.indexOf(">>>") + 1 to tempList.length - 1) {
        for(name <- searchQueue.keySet) 
          if(tempList(i) == (name) || tempList(i) == name + "$" 
              || startList.contains((lineTable(length - pos + 1) + tempList(tempList.indexOf(">>>") + 1)))) { 
            touched = true
            removeQueue.add(name)
          }
        for(name <- removeQueue) {
          searchQueue(name) = searchQueue(name) - 1
          if(searchQueue(name) == 0) {
            searchQueue -= name
          }
        }
        removeQueue.clear()
      }
      if(touched) {
        outputFile.write(lineTable(length - pos) + "\n" + lineTable(length - pos + 1))
        outputFile.write("\n")
        outputFile.flush()
        for(i <- 0 to tempList.indexOf(">>>") - 1) {
          searchQueue += ((tempList(i), 1))
        }
        touched = false
      }
      else {
        bufferQueue.enqueue(tempList)
        if(bufferQueue.length > 1000)
          bufferQueue.dequeue()
        for(bufferline <- bufferQueue)
          for(i <- bufferline.indexOf(">>>") to bufferline.length - 1) {
            for(name <- searchQueue.keySet) {
              if(bufferline(i) == name) {
                touched = true
                removeQueue.add(name)
              }
            }
            for(name <- removeQueue) {
              searchQueue(name) = searchQueue(name) - 1
              if(searchQueue(name) == 0) {
                searchQueue -= name
              }
            }
            removeQueue.clear()
            if(touched) {
              outputFile.write(lineTable(length - pos) + "\n" + lineTable(length - pos + 1))
              outputFile.write("\n")
              outputFile.flush()
              for(i <- 0 to tempList.indexOf(">>>") - 1) {
                searchQueue += ((tempList(i), 1))
              }
              touched = false
            }
          }
      }
     }
    }
    outputFile.close()
  }
}

object Searcher {
  def main(args : Array[String]) {
      val searcher = new Searcher(args(0), args(1))
      searcher.readAll()
      searcher.search()
  }
}