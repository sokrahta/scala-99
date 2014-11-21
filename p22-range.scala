def range(i: Int, k: Int): List[Int] = {
  @annotation.tailrec
  def go(i: Int, k: Int, acc: List[Int]): List[Int] = {
    if (i >= k) acc
    else go(i, k-1, k :: acc)
  }
  go(i,k,Nil)
}

range(4, 9)
//res0: List[Int] = List(4, 5, 6, 7, 8, 9)

