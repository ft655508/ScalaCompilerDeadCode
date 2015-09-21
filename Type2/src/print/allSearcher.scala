/*
 * This tool carries on search on the data flow file. Unlike Searcher.scala,
 * allSearcher takes every node on the right hand side in the data flow as starting
 * nodes, and search from all these starting nodes until the file is finished or the 
 * search is broken. It prints out continuous data flow sequences with a size larger 
 * than the threshold value. allSearchers takes one argument, the name of data flow file.
 */

import scala.io.Source
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import java.io._
import scala.Predef
import collection.mutable.ArrayBuffer
import collection.mutable.Queue
import collection.mutable.Stack

class allSearcher(dataFlow : String) {
  val lineTable : Queue[String] = Queue[String]()  
  val outputFile = new java.io.FileWriter(dataFlow + "Result", true)
  val searchQueue : Map[String, Stack[Int]] = Map[String, Stack[Int]]()
  val dataFlowQueue : Map[String, Stack[Queue[String]]] = Map[String, Stack[Queue[String]]]()
  val removeQueue : Set[String] = Set[String]()
  val bufferQueue : Queue[Array[String]] = Queue[Array[String]]()
  val startList : ArrayBuffer[String] = ArrayBuffer[String]()
  val lineSet : Set[String] = Set[String]()
  var readed = 0
  
  def readAll() {
    for(line <- Source.fromFile(dataFlow).getLines) {
      lineTable.enqueue(line)
      readed = readed + 1
      if(readed % 1000000 == 0)
        println(readed)
    }
    println("read finish")
  }
  
  def search() {
    val length = lineTable.length - 1
    var touched : Boolean = false
    var start = -1
    var startline = 0
    println("starting search")
    for(pos <- 0 to length) {
      val templine = lineTable.dequeue()
      val tempList = templine.trim.split(" ")
      for(i <- 0 to tempList.length - 1) tempList(i) = tempList(i).trim
      if(tempList.contains(">>>")) {
      for(i <- tempList.indexOf(">>>") + 1 to tempList.length - 1) {
        for(name <- searchQueue.keySet) 
          if(tempList(i) == (name) || tempList(i) == name + "$" 
              /*|| startList.contains((lineTable(length - pos + 1) + tempList(tempList.indexOf(">>>") + 1)))*/) { 
            touched = true
            removeQueue.add(name)
            val level = searchQueue(name).pop()
            val levellines = dataFlowQueue(name).pop()
            levellines.enqueue(templine)
            if(levellines.length > 5) {
              for(lline <- levellines) 
                if(!lineSet.contains(lline)) {
                  lineSet += lline
                  outputFile.write(lline + "\n")
                }
              outputFile.flush()
              levellines.clear()
            }
            for(inname <- tempList.slice(0, tempList.indexOf(">>>"))) {
              if(searchQueue.keySet.contains(inname)) {
               searchQueue(inname).push(level + 1)
               dataFlowQueue(inname).push(levellines)
              }
              else {
                searchQueue += ((inname, Stack(level + 1)))
                dataFlowQueue += ((inname, Stack(levellines)))
              }
            }
          }
        for(name <- removeQueue) {
          if(searchQueue(name).length == 0) {
            searchQueue -= name
            dataFlowQueue -= name
            //println("removing " + name)
          }
        }
        removeQueue.clear()
      }
      if(!touched) {
        bufferQueue.enqueue(tempList)
        if(bufferQueue.length > 100) {
          val newin = bufferQueue.dequeue()
          for(inname <- newin.slice(0, newin.indexOf(">>>")))
            if(searchQueue.keySet.contains(inname)) {
               searchQueue(inname).push(1)
               dataFlowQueue(inname).push(Queue(templine))
            }
              else {
                searchQueue += ((inname, Stack(1)))
                dataFlowQueue += ((inname, Stack(Queue(templine))))
              }
        }
        for(bufferline <- bufferQueue)
          for(i <- bufferline.indexOf(">>>") to bufferline.length - 1) {
            for(name <- searchQueue.keySet) {
              if(bufferline(i) == name) {
                touched = true
                removeQueue.add(name)
                val level = searchQueue(name).pop()
                val levelline = dataFlowQueue(name).pop()
                levelline.enqueue(templine)
                if(levelline.length > 5) {
              for(lline <- levelline) 
                if(!lineSet.contains(lline)) {
                  lineSet += lline
                  outputFile.write(lline + "\n")
                }
              outputFile.flush()
              levelline.clear()
            }
                for(inname <- tempList.slice(0, tempList.indexOf(">>>"))) {
                  if(searchQueue.keySet.contains(inname)) {
                   searchQueue(inname).push(level + 1)
                   dataFlowQueue(inname).push(levelline)
                  }
                  else {
                    searchQueue += ((inname, Stack(level + 1)))
                    dataFlowQueue += ((inname, Stack(levelline)))
                  }
                }
              }
            }
            for(name <- removeQueue) {
              if(searchQueue(name).length == 0) {
                searchQueue -= name
                dataFlowQueue -= name
                //println("removing " + name)
              }
            }
            removeQueue.clear()
          }
      }
      
        touched = false
     }
    }
  }
  
  def result() {
    for(name <- searchQueue.keySet)
      for(length <- dataFlowQueue(name)) {
        if(length.length > 10) {
          for(line <- length) {
            if(!lineSet.contains(line)) {
              lineSet += line
              outputFile.write(line + "\n")
            }
          }
            outputFile.flush()
        }
      }
    outputFile.close()
  }
}

object reduceRedun {
  def reduce(filename : String) {
    val lineSet : Set[String] = Set[String]()
    val outputFile = new java.io.FileWriter(filename + "Reduced", true)
    for(line <- Source.fromFile(filename).getLines()) {
      if(!lineSet.contains(line)) {
        lineSet += line
        outputFile.write(line + "\n")
      }
    }
    outputFile.flush()
    outputFile.close()
  }
}

object allSearcher {
  def main(args : Array[String]) {
      val searcher = new allSearcher(args(0))
      val reducer = reduceRedun
      reduceRedun.reduce(args(0))
      searcher.readAll()
      searcher.search()
      searcher.outputFile.close()
  }
}