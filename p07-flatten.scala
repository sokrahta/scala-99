def flatten(as: List[Any]): List[Any] = as flatMap {
  case as: List[_] => flatten(as)
  case a => List(a)
}

flatten(List(List(1,1),2,List(3,List(5,8))))
//res0: List[Any] = List(1, 1, 2, 3, 5, 8)

