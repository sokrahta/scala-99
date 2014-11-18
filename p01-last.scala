def last[A](as: List[A]): A = as match {
  case h :: Nil => h
  case _ :: t => last(t)
  case _ => throw new NoSuchElementException
}

