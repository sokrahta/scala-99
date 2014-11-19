def encode[A](as: List[A]): List[(Int,A)] = {
  if(as.isEmpty) Nil
  else {
    val (l, r) = as span { _ == as.head }
    if (r == Nil) List((l.length, l(0)))
    else (l.length, l(0)) :: encode(r)
  }
}


encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

