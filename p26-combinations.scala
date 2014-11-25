def removeAt[A](k: Int, as: List[A]): (List[A], A) = {
  if (k > as.length) throw new NoSuchElementException
  else {
    val t = as.drop(k)
    (as.take(k) ::: t.drop(1), t(0))
  }
}

def combinations[A](n: Int, as: List[A]): List[List[A]] = {
  def go(n: Int, as: List[A], acc: List[A]): List[List[A]] = {
    if (n <= 0 || as.isEmpty) acc :: Nil
    else {
      as.zipWithIndex
        .flatMap(t =>  go(n-1, removeAt(t._2, as)._1, t._1 :: acc) )
    }
  }
  go(n, as, Nil)
}

combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
//res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...

