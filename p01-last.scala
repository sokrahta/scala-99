def last[A](as: List[A]): A = as match {
  case h :: Nil => h
  case _ :: t => last(t)
  case _ => throw new NoSuchElementException
}

last(List(1,1,2,3,5,8))
//res0: Int = 8
