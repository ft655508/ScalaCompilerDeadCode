package counter

object test{
  println("Hello Scala")
  println("Hello Scala")
}

class Counter{
  
  def f(x:Int)={
    x+5
    val myvalue = x + 6
    x + 6
  }
  println("Counter Initialization")
}


class newclass(x : Int){
  def classname = new anotherclass(0)
  def getValue() : Int = 9
  def value = x
  def getanothermethod = classname.anothermethod(x)
}

class anotherclass(y : Int) extends newclass(y){
  def anothermethod(z : Int) = 2
}

abstract class abstractClass{
  def f(t: Int) : Int
  def m : Int
  val a : Int = {println("In abstract valdef"); 9}
  def abort(pos: Int, msg: String) = "a"
}

class implement extends abstractClass{
  val b : Int = 7
  def f(t : Int) : Int = t + 1
  override def m = f(1)
  def n = new newclass(0).getValue()
}

trait List[T] {
  def isEmpty : Boolean = false
  println("Here is a trait")
  def head : T
  def tail : List[T]
  val oldval = 2
  var s : String = "a"
  def myval = 2
  val newval = {println("newval created in trait"); 0}
}

class Cons[T](val head : T, val tail : List[T]) extends List[T] {
  override def isEmpty = false
}

class Nil[T] extends List[T] {
  //override def isEmpty : Boolean = true;
  def head : Nothing = throw new NoSuchElementException("Nil.head")
  def tail : Nothing = throw new NoSuchElementException("Nil.head")
}

class Ni[T] extends List[T] {
  //override def isEmpty : Boolean = true;
  def head : Nothing = throw new NoSuchElementException("Nil.head")
  def tail : Nothing = throw new NoSuchElementException("Nil.head")
}

object HelloList {
  def nth[T](n : Int, list : List[T]) {
    if(list.equals(Nil)) throw new IndexOutOfBoundsException("Index out of range")
    else if (n == 0) list.head
    else nth(n - 1, list.tail)
  }
}

