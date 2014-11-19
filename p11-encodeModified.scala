def encodeModified[A](as: List[A]): List[Any] = {
  if(as.isEmpty) Nil
  else {
    val (l, r) = as span { _ == as.head }
    val elem = if (l.length > 1) (l.length, l(0)) else l(0)
    if (r == Nil) List(elem)
    else (elem) :: encodeModified(r)
  }
}

encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

