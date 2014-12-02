def lsort[A](as: List[List[A]]): List[List[A]] = {
  as map {a => (a, a.length)} sortWith( (al1,al2) => al1._2.compareTo(al2._2) < 0) map {_._1}
}

lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))

