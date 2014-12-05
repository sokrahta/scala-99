class S99Logic(a: Boolean) {
  import S99Logic.boolean2S99Logic

  def not: Boolean = a match {
    case true  => false
    case false => true
  }
  def and (b: Boolean): Boolean = (a,b) match {
    case (true,true) => true
    case _ => false
  }
  def or  (b: Boolean): Boolean = (a,b) match {
    case (true,_) => true
    case (_,true) => true
    case _ => false
  }

  def nand(b: Boolean): Boolean = (a and b).not
  def nor (b: Boolean): Boolean = (a or a).not
  def xor (b: Boolean): Boolean = (a equ b).not
  def impl(b: Boolean): Boolean = a.not or b
  def equ (b: Boolean): Boolean = (a and b) or (a.not and b.not)
}

object S99Logic {
  implicit def boolean2S99Logic(a: Boolean): S99Logic = new S99Logic(a)


  def table2(fname: String, f: (Boolean, Boolean) => Boolean): Unit = {
    def printone(a: String, b: String, r: String) = println("%-5s %-5s %-5s = %s".format(a,fname,b,r))
    printone("A","B","result")
    for {a <- List(true,false);
         b <- List(true,false)}
      {printone(a.toString, b.toString, f(a,b).toString)}
  }
}

object Exercises {
  import S99Logic._
  import S99Logic.boolean2S99Logic
  def run {
    table2("and",  _ and _)
    table2("or",   _ or _)
    table2("nand", _ nand _)
    table2("nor",  _ nor _)
    table2("xor",  _ xor _)
    table2("impl", _ impl _)
    table2("equ",  _ equ _)
  }
}

Exercises.run

