def isPalindrome[A](as: List[A]): Boolean = {
  def go(rs: List[A], as: List[A]): Boolean = (rs, as) match {
    case (a::Nil, c::Nil) => a==c
    case (a::b, c::d) => if (a==c) go(b,d) else false
    case (_,_) => false
  }
  go(as.reverse, as)
}

isPalindrome(List(1,2,3,2,1))
//res0: Boolean = true

