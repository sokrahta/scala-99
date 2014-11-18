def nth[A](n: Int, as: List[A]): A = {
  def go(n: Int, as: List[A]): A = {
    as match {
      case h :: t => if (n == 0) h else go(n-1, t)
      case _ => throw new NoSuchElementException
    }
  }
  if (n < 0) throw new NoSuchElementException
  else go(n, as)
}

nth(2, List(1,1,2,3,5,8))

