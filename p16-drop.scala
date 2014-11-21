def drop[A](n: Int, as: List[A]): List[A] = {
  as.zipWithIndex.filter(a => (a._2+1) % n != 0).map(_._1)
}

drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

