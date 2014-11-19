def compress[A](as: List[A]): List[A] = {
  def go(rs: List[A], a: A, as: List[A]): List[A] = as match {
    case h::Nil => if (a==h) rs else h::rs
    case h::t => if (a==h) go(rs,a,t) else go(h::rs,h,t)
    case _ => throw new NoSuchElementException
  }
  if (as.length <= 1) as
  else go(as(0)::Nil, as(0), as).reverse
}

compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

