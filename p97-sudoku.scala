def deserialize(s: String): Array[Array[Char]] =
  s.trim.replaceAll("[|]","").split("(\r)?\n").zipWithIndex.filter(_._2%2==0).map(_._1.toCharArray.filter(_!=' '))

def serialize(a: Array[Array[Char]]): String =
  a.map(_.sliding(3,3).map(_.mkString("  ")).mkString(" | "))
    .sliding(3,3).map(_.mkString("\n        |         |        \n")).mkString("\n--------+---------+--------\n")

def row[T](p: Array[Array[T]], x: Int): List[T] = p(x).toList
def col[T](p: Array[Array[T]], y: Int): List[T] = p.map(_(y)).toList
def box[T](p: Array[Array[T]], x: Int, y: Int): List[T] =
  p.zipWithIndex.filter(_._2/3==x/3).map(_._1).flatMap(_.zipWithIndex.filter(_._2/3==y/3).map(_._1)).toList
def box[T](p: Array[Array[T]], n: Int): List[T] = box(p, n%3, n/3)

val series = List('1', '2', '3', '4', '5', '6', '7', '8', '9')

def candidates(p: Array[Array[Char]]): Array[Array[List[Char]]] = {
  var c = Array.fill(9)(Array.fill(9)(series))
  for(
    x <- 0 to 8;
    y <- 0 to 8)
  yield {
    val (v, xb, yb) = (p(x)(y), x/3*3, y/3*3)
    if (v!='.') {
      c(x)(y) = List(v)
      for(x1 <- (0 to 8).filter(_!=x)) yield c(x1)(y) = c(x1)(y).filter(_!=v)
      for(y1 <- (0 to 8).filter(_!=y)) yield c(x)(y1) = c(x)(y1).filter(_!=v)
      for(
        x1 <- (xb to xb+2);
        y1 <- (yb to yb+2))
      yield if (x1!=x || y1!=y) c(x1)(y1) = c(x1)(y1).filter(_!=v)
    }
  }
  c
}

def singles(p: Array[Array[Char]]): (Int, Array[Array[Char]]) = {
  var (count, cs) = (0, candidates(p))
  for(
    x <- 0 to 8;
    y <- 0 to 8)
  yield {
    if (p(x)(y)=='.') {
      val ws = cs(x)(y)
      if (ws.length == 1) {
        count+=1
        p(x)(y) = ws(0)
      }
    }
  }
  (count,p)
}

def hiddenSingles(p: Array[Array[Char]]): (Int, Array[Array[Char]]) = {
  var (count, cs) = (0, candidates(p))
  for(
    x <- 0 to 8;
    y <- 0 to 8)
  yield {
    if (p(x)(y)=='.') {
      val (r, c, b) = (row(cs,x), col(cs,y), box(cs,x,y))
      val rw = r.flatMap(a=>a).toList.groupBy(a=>a).map(a => (a._1, a._2.length)).filter(_._2==1).map(_._1).toList
      val cw = c.flatMap(a=>a).toList.groupBy(a=>a).map(a => (a._1, a._2.length)).filter(_._2==1).map(_._1).toList
      val bw = b.flatMap(a=>a).toList.groupBy(a=>a).map(a => (a._1, a._2.length)).filter(_._2==1).map(_._1).toList
      val rws = cs(x)(y).filter(a => rw.contains(a))
      val cws = cs(x)(y).filter(a => cw.contains(a))
      val bws = cs(x)(y).filter(a => bw.contains(a))
      if (rws.length == 1) {
        count+=1
        p(x)(y) = rws(0)
      } else if (cws.length == 1) {
        count+=1
        p(x)(y) = cws(0)
      } else if (bws.length == 1) {
        count+=1
        p(x)(y) = bws(0)
      }
    }
  }
  (count,p)
}

def nakedPairs(cs: Array[Array[List[Char]]]): (Int,Array[Array[List[Char]]]) = {
  @annotation.tailrec
  def go(count: Int, acc: Array[Array[List[Char]]]): (Int,Array[Array[List[Char]]] = {
    var changes = 0
    for(n <- 0 to 8) yield {
      val (r, c, b) = (row(cs,n), col(cs,n), box(cs,n))
      val rw = r.groupBy(a=>a).filter(a => a._1.length==2 && a._2.length==2)
      val cw = c.groupBy(a=>a).filter(a => a._1.length==2 && a._2.length==2)
      val bw = b.groupBy(a=>a).filter(a => a._1.length==2 && a._2.length==2)
      
    }
    if (changes > 0) go(count+changes,cs) else (count,cs)
  }
  go(0,cs)
}

def solve(problem: String): String = {
  var iter: Int = 0
  @annotation.tailrec
  def go(acc: Array[Array[Char]]): Array[Array[Char]] = {
    iter+=1
    if (acc.flatMap(_.find(_=='.')).isEmpty) acc
    else {
      //TODO: add solving rules for more complex sudoku problems
      val (count1,next1) = singles(acc)
      val (count2,next2) = hiddenSingles(next1)
      if (List(count1,count2).sum == 0) acc else go(next2)
    }
  }
  val solved = serialize(go(deserialize(problem)))
  println(s"solve took %d iterations".format(iter))
  solved
}

val sudokuSolved = solve("""
.  .  4 | 8  .  . | .  1  7
        |         |        
6  7  . | 9  .  . | .  .  .
        |         |        
5  .  8 | .  3  . | .  .  4
--------+---------+--------
3  .  . | 7  4  . | 1  .  .
        |         |        
.  6  9 | .  .  . | 7  8  .
        |         |        
.  .  1 | .  6  9 | .  .  5
--------+---------+--------
1  .  . | .  8  . | 3  .  6
        |         |        
.  .  . | .  .  6 | .  9  1
        |         |        
2  4  . | .  .  1 | 5  .  .
""")
val testSolution = """
9  3  4 | 8  2  5 | 6  1  7
        |         |
6  7  2 | 9  1  4 | 8  5  3
        |         |
5  1  8 | 6  3  7 | 9  2  4
--------+---------+--------
3  2  5 | 7  4  8 | 1  6  9
        |         |
4  6  9 | 1  5  3 | 7  8  2
        |         |
7  8  1 | 2  6  9 | 4  3  5
--------+---------+--------
1  9  7 | 5  8  2 | 3  4  6
        |         |
8  5  3 | 4  7  6 | 2  9  1
        |         |
2  4  6 | 3  9  1 | 5  7  8
"""

val sudokuSolved2 = solve("""
.  .  . | .  .  4 | .  2  8
        |         |        
4  .  6 | .  .  . | .  .  5
        |         |        
1  .  . | .  3  . | 6  .  .
--------+---------+--------
.  .  . | 3  .  1 | .  .  .
        |         |        
.  8  7 | .  .  . | 1  4  .
        |         |        
.  .  . | 7  .  9 | .  .  .
--------+---------+--------
.  .  2 | .  1  . | .  .  3
        |         |        
9  .  . | .  .  . | 5  .  7
        |         |        
6  7  . | 4  .  . | .  .  .
""")

