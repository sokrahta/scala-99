def duplicate[A](as: List[A]): List[A] = as match {
  case h :: t => h :: h :: duplicate(t)
  case _ => Nil
}

duplicate(List('a, 'b, 'c, 'c, 'd))
//res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

