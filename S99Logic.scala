object S99Logic {
  def and (a: Boolean, b: Boolean): Boolean = a && b
  def or  (a: Boolean, b: Boolean): Boolean = a || b
  def nand(a: Boolean, b: Boolean): Boolean = !and(a,b)
  def nor (a: Boolean, b: Boolean): Boolean = !or(a,b)
  def xor (a: Boolean, b: Boolean): Boolean = !equ(a,b)
  def impl(a: Boolean, b: Boolean): Boolean = or(!a,b)
  def equ (a: Boolean, b: Boolean): Boolean = a==b

  def table2(fname: String, f: (Boolean,Boolean) => Boolean): Unit = {
    def printone(a: String, b: String, r: String) = println("%-5s %-5s %-5s = %s".format(a,fname,b,r))
    printone("A","B","result")
    printone("true","true",f(true,true).toString)
    printone("true","false",f(true,false).toString)
    printone("false","true",f(false,true).toString)
    printone("false","false",f(false,false).toString)
  }
}

object Exercises {
  import S99Logic._
  def run {
    table2("and", and)
    table2("or", or)
    table2("nand", nand)
    table2("nor", nor)
    table2("xor", xor)
    table2("impl", impl)
    table2("equ", equ)
  }
}

Exercises.run

