def slice[A](i: Int, k: Int, as: List[A]): List[A] = {
  //as.drop(i).take(k-i)
  @annotation.tailrec
  def go(i: Int, k: Int, as: List[A], acc: List[A]): List[A] = as match {
    case Nil => acc
    case h::t => {
      if (i > 0) go(i-1, k-1, t, acc)
      else if (k > 0) go(i, k-1, t, h::acc)
      else acc
    }
  }
  go(i, k, as, Nil).reverse
}

slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: List[Symbol] = List('d, 'e, 'f, 'g)

