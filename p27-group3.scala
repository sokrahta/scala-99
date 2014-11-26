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

def group3[A](as: List[A]): List[List[List[A]]] = {
  if (as.length != 9) Nil
  combinations(9, as).map(cs => List(cs.slice(0,2), cs.slice(2,5), cs.slice(5,9)))
}

group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
//res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...

