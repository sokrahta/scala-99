def length[A](as: List[A]): Int = {
  def go(n: Int, as: List[A]): Int = {
    as match {
      case _ :: Nil => n+1
      case _ :: t => go(n+1, t)
      case _ => n
    }
  }
  go(0, as)
}

length(List(1,1,2,3,5,8))
//res0: Int = 6

