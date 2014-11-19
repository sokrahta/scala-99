def penultimate[A](as: List[A]): A = as match {
  case h :: t :: Nil => h
  case _ :: t => penultimate(t)
  case _ => throw new NoSuchElementException
}

penultimate(List(1,1,2,3,5,8))
//res0: Int = 5

