import util.Random

def removeAt[A](k: Int, as: List[A]): (List[A], A) = {
  if (k > as.length) throw new NoSuchElementException
  else {
    val t = as.drop(k)
    (as.take(k) ::: t.drop(1), t(0))
  }
}

def randomSelect[A](count: Int, as: List[A]): List[A] = {
  def go(count: Int, as: List[A], acc: List[A]): List[A] = {
    if (count <= 0) acc
    else {
      val i = Random.nextInt(as.length)
      val (bs, a) = removeAt(i, as)
      go(count-1, bs, a::acc)
    }
  }
  go(count, as, Nil)
}

def randomPermute[A](as: List[A]): List[A] = {
  randomSelect(as.length, as)
}

randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
//res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)

