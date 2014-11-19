def reverse[A](as: List[A]): List[A] = {
  def go(rs: List[A], as: List[A]): List[A] = as match {
    case h :: Nil => h :: rs
    case h :: t => go(h :: rs, t)
    case _ => Nil
  }
  go(Nil, as)
}

reverse(List(1,1,2,3,5,8))
//res0: List[Int] = List(8, 5, 3, 2, 1, 1)

