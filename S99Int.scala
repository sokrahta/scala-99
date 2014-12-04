class S99Int(val start: Int) {
  import S99Int._
  
  override def toString: String = start.toString

  def isPrime: Boolean = (start > 1) && (primes takeWhile {_<=Math.sqrt(start)} forall {start % _ != 0})
  def isCoprimeTo(other: S99Int): Boolean = (gcd(this, other) == 1)
  def totient: Int = (integers takeWhile {_<=start} filter {this.isCoprimeTo(_)}).length
  def primeFactors: List[Int] = {
    def go(i: Int, acc: List[Int]): List[Int] = {
      if (i <= 1) acc
      else primes find {i % _ == 0} match {
          case Some(x) => go(i / x, x::acc)
          case _ => acc
        }
    }
    go(start, Nil)
  }
  def primeFactorMultiplicity: Map[Int,Int] =
    this.primeFactors groupBy(x => x) map {case (x,xs) => x -> xs.length}
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  implicit def S99Int2Int(s: S99Int): Int = s.start
  
  val integers = Stream.cons(1, Stream.from(2,1))
  val primes = Stream.cons(2, Stream.from(3,2) filter {_.isPrime})
  
  def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
}

import S99Int.{int2S99Int, S99Int2Int}

7.isPrime //res0: Boolean = true
S99Int.gcd(36, 63) //res0: Int = 9
35.isCoprimeTo(64) //res0: Boolean = true
10.totient //res0: Int = 4
315.primeFactors //res0: List[Int] = List(3, 3, 5, 7)
315.primeFactorMultiplicity //res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)

