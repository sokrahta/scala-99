def duplicateN[A](n: Int, as: List[A]): List[A] = as match {
  case h :: t => List.fill(n)(h) ::: duplicateN(n, t)
  case _ => Nil
}

duplicateN(3, List('a, 'b, 'c, 'c, 'd))
//res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

