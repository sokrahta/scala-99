def reverse[A](as: List[A]): List[A] = {
  def go(rs: List[A], as: List[A]): List[A] = as match {
    case h :: Nil => h :: rs
    case h :: t => go(h :: rs, t)
    case _ => Nil
  }
  go(Nil, as)
}

