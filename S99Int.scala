class S99Int(val start: Int) {
  import S99Int._
  
  override def toString: String = start.toString

  def isPrime: Boolean = (start > 1) && (primes takeWhile {_<=Math.sqrt(start)} forall {start % _ != 0})
  def isCoprimeTo(other: S99Int): Boolean = (gcd(this, other) == 1)
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  implicit def S99Int2Int(s: S99Int): Int = s.start
  
  val primes = Stream.cons(2, Stream.from(3,2) filter {_.isPrime})
  
  def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
}

import S99Int.{int2S99Int, S99Int2Int}

7.isPrime //res0: Boolean = true
S99Int.gcd(36, 63) //res0: Int = 9
35.isCoprimeTo(64) //res0: Boolean = true

