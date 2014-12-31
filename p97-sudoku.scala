def deserialize(s: String): Array[Array[Char]] =
  s.replaceAll("[|]","").split("(\r)?\n").zipWithIndex.filter(_._2%2==1).map(_._1.toCharArray.filter(_!=' '))

def serialize(a: Array[Array[Char]]): String =
  a.map(_.sliding(3,3).map(_.mkString("  ")).mkString(" | "))
    .sliding(3,3).map(_.mkString("\n        |         |        \n")).mkString("\n--------+---------+--------\n")

def row(p: Array[Array[Char]], x: Int): Array[Char] =
  p(x)

def col(p: Array[Array[Char]], y: Int): Array[Char] =
  p.map(_(y))

def box(p: Array[Array[Char]], x: Int, y: Int): Array[Char] =
  p.zipWithIndex.filter(_._2/3==x/3).map(_._1).flatMap(_.zipWithIndex.filter(_._2/3==y/3).map(_._1))

val series = List('1', '2', '3', '4', '5', '6', '7', '8', '9')

def singles(p: Array[Array[Char]]): (Int, Array[Array[Char]]) = {
  var count: Int = 0
  for(
    x <- 0 to 8;
    y <- 0 to 8)
  yield {
    if (p(x)(y)=='.') {
      val (r, c, b) = (row(p,x), col(p,y), box(p,x,y))
      val w = series.filter(a => !r.contains(a) && !c.contains(a) && !b.contains(a))
      if (w.isEmpty) {
        throw new Exception("overconstrained")
      }
      else if (w.length == 1) {
        count+=1
        p(x)(y) = w(0)
      }
    }
  }
  (count,p)
}

def solve(problem: String): String = {
  @annotation.tailrec
  def go(acc: Array[Array[Char]]): Array[Array[Char]] = {
    if (acc.flatMap(_.find(_=='.')).isEmpty) acc
    else {
      //TODO: add solving rules for more complex sudoku problems
      val (count,next) = singles(acc)
      if (count == 0) throw new Exception("need more complex solution")
      go(next)
    }
  }
  serialize(go(deserialize(problem)))
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

