def split[A](n: Int, as: List[A]): (List[A],List[A]) = {
  //(as.take(n), as.drop(n))
  @annotation.tailrec
  def go[A](m: Int, acc: List[A], rem: List[A]): (List[A],List[A]) = {
    if (m <= 0) (acc.reverse, rem)
    else {
      rem match {
        case Nil => (acc.reverse, Nil)
        case h::t => go(m-1, h::acc, t)
      }
    }
  }
  go(n, Nil, as)
}

split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

