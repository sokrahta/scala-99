def encodeDirect[A](as: List[A]): List[(Int,A)] = {
  if (as.isEmpty) Nil
  else {
    val (packed, next) = as span { _ == as.head }
    (packed.length, packed(0)) :: encodeDirect(next)
  }
}

encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

