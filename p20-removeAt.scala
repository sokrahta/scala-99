def removeAt[A](k: Int, as: List[A]): (List[A], A) = {
  if (k > as.length) throw new NoSuchElementException
  else {
    val t = as.drop(k)
    (as.take(k) ::: t.drop(1), t(0))
  }
}

removeAt(1, List('a, 'b, 'c, 'd))
//res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

