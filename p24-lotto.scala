import util.Random

def lotto(n: Int, m: Int): List[Int] = {
  if (m <= 1) Nil
  def go(n: Int, m: Int, acc: List[Int]): List[Int] = {
    if (n <= 0) acc
    else {
      val x = Random.nextInt(m)+1
      go(n-1, m, x::acc)
    }
  }
  go(n, m, Nil)
}

lotto(6, 49)
//res0: List[Int] = List(23, 1, 17, 33, 21, 37)

