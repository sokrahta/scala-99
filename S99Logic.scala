object S99Logic {
  def and (a: Boolean, b: Boolean): Boolean = a && b
  def or  (a: Boolean, b: Boolean): Boolean = a || b
  def nand(a: Boolean, b: Boolean): Boolean = !and(a,b)
  def nor (a: Boolean, b: Boolean): Boolean = !or(a,b)
  def xor (a: Boolean, b: Boolean): Boolean = !equ(a,b)
  def impl(a: Boolean, b: Boolean): Boolean = or(!a,b)
  def equ (a: Boolean, b: Boolean): Boolean = a==b
}

object Exercises {
  import S99Logic._
  def run {
    println(and(true,true)) //res0: Boolean = true
    println(or(true,true))
    println(nand(true,true))
    println(nor(true,true))
    println(xor(true,true))
    println(impl(true,true))
    println(equ(true,true))
  }
}

Exercises.run

