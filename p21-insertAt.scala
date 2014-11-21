def insertAt[A](value: A, i: Int, as: List[A]): List[A] = {
  as.take(i) ::: value :: as.drop(i)
}

insertAt('new, 1, List('a, 'b, 'c, 'd))
//res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)

