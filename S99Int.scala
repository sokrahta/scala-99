class S99Int(val start: Int) {
  import S99Int._
  
  override def toString: String = start.toString

  def isPrime: Boolean = (start > 1) && (primes takeWhile {_<=Math.sqrt(start)} forall {start % _ != 0})
  def isCoprimeTo(other: S99Int): Boolean = (gcd(this, other) == 1)
  def totient1: Int = (integers takeWhile {_<=start} filter {this.isCoprimeTo(_)}).length
  def totient: Int =
    start.primeFactorMultiplicity.foldLeft(1) { (r,f) =>
      f match {case (p,m) => r * (p-1) * Math.pow(p, m-1).toInt }
    }
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
  def goldbach: (Int,Int) =
    primes find {x => (start-x).isPrime} match {
      case Some(x) => (x, start-x)
      case _ => throw new NoSuchElementException
    }
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  implicit def S99Int2Int(s: S99Int): Int = s.start
  
  val integers = Stream.cons(1, Stream.from(2,1))
  val primes = Stream.cons(2, Stream.from(3,2) filter {_.isPrime})
  def listPrimesinRange(ir: Range): List[Int] =
    primes takeWhile {_<=ir.max} filter {_>=ir.min} toList
  
  def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
}

object P38 {
  import S99Int._

  def time[A](label: String)(block: => A): A = {
    val now = System.currentTimeMillis()
    val ret = block
    println(label + ": " + (System.currentTimeMillis() - now) + " ms.")
    ret
  }

  def test(n: Int) {
    time("Preload primes") { primes takeWhile { _ <= Math.sqrt(n) } force }
    time("P34 (" + n + ")") { n.totient1 }
    time("P37 (" + n + ")") { n.totient }
  }
}

object Exercises {
  import S99Int.{int2S99Int, S99Int2Int}

  def run  = {
    7.isPrime //res0: Boolean = true
    S99Int.gcd(36, 63) //res0: Int = 9
    35.isCoprimeTo(64) //res0: Boolean = true
    10.totient //res0: Int = 4
    315.primeFactors //res0: List[Int] = List(3, 3, 5, 7)
    315.primeFactorMultiplicity //res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)
    S99Int.listPrimesinRange(7 to 31) //res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)
    28.goldbach //res0: (Int, Int) = (5,23)
  }
}

P38.test(100900)
Exercises.run

