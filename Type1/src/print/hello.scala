import counter._

object hello{
  def main(args:Array[String]) {
    println("Hello Scala")
    calc(3)
    calc(5)
    def m = 4
    var x = 
    {
      calc({println("a"); calc(3)})
      calc(7)
    }
    
    if(x > 0){
      println("x > 0")
    }
    
    var newcnt : Counter = new Counter
    
    x = 
      calc(4)
      
      println({println("a");             
      new newclass(0)}.classname)
      
      println(new newclass(0).getValue)
      
      newcnt.f(1)
      
    try{
    var y = x match {
      case 1 => 
        calc(5)
      case 2 => 2
      
      case t => throw new IndexOutOfBoundsException("Not Match") 
    }} 
    catch {
      case m : Throwable => 
    }
    x = 8
    var z = 10
  }
  def calc(x:Int):Int = {/*return*/ x+1; /*x*/}
  
  def signal(msg: String): String = "a"
  
  val i : implement = null
  var im : implement = new implement
  
  //im.abort(1, "a")
  
  println(im.f(1))
  
  im = new implement
  
  println(im.m)
  
  println(im.abort(3, "a"))
  
  val is = {println("a"); new newclass(1)}
  {
    println("a")
    println({println("a"); is.getValue})
  }
  
  val b = 0
  
  var t : Nil[Int] = new Nil[Int]
  
  println(t.isEmpty)
  
  println("printa")
  
  var nil : Nil[Int] = new Nil[Int]
  
  println(nil.isEmpty)
  
}

