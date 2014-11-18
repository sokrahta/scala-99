def penultimate[A](as: List[A]): A = as match {
  case h :: t :: Nil => h
  case _ :: t => penultimate(t)
  case _ => throw new NoSuchElementException
}

