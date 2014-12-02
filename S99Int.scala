class S99Int(val start: Int) {
  import S99Int._
  
  override def toString: String = start.toString
  
  def isPrime: Boolean = (start > 1) && (primes takeWhile {_<=Math.sqrt(start)} forall {start % _ != 0})
    
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  
  val primes = Stream.cons(2, Stream.from(3,2) filter {_.isPrime})
}

import S99Int.int2S99Int

7.isPrime

