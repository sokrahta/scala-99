private def lsortpart[A](as: List[List[A]]): List[(List[A],Int)] =
  as map {a => (a, a.length)} sortWith(_._2 < _._2)
  
def lsort[A](as: List[List[A]]): List[List[A]] =
  lsortpart(as) map {_._1}

lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))

def lsortFreq[A](as: List[List[A]]): List[List[A]] = {
  val ms: Map[Int, List[(List[A], Int)]] = lsortpart(as) groupBy(_._2)
  val ls: List[List[(List[A], Int)]] = ms map {case (k, j) => j map {case (x) => (x._1,j.length)} } toList
  val fs: List[List[A]] = ls flatMap(a => a) sortWith(_._2 < _._2) map {_._1}
  fs
}

lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))

