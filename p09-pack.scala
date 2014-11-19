def pack[A](as: List[A]): List[List[A]] = {
  if(as.isEmpty) List(List())
  else {
    val (l, r) = as span { _ == as.head }
    if (r == Nil) List(l)
    else l :: pack(r)
  }
}

pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

