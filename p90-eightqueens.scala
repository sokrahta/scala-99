def isValid(queens: List[Int]) = {//: Seq[(Int,Int,Int)] = {
  if (queens.length != 8 || queens.min < 0 || queens.max > 7) throw new Exception("chess boards are 8x8, 0 indexed")
  val chessboard = for (
    x <- 0 to 7;
    y <- 0 to 7) yield {
      (x, y, x-y, x-7+y, if (queens(x)==y) 'q' else '_')
  }
  val rows = for (row <- 0 to 7)  yield {chessboard.filter(a => a._1 == row && a._5 == 'q').length == 1}
  val cols = for (col <- 0 to 7)  yield {chessboard.filter(a => a._2 == col && a._5 == 'q').length == 1}
  val dups = for (dup <- -7 to 7) yield {chessboard.filter(a => a._3 == dup && a._5 == 'q').length <= 1}
  val ddns = for (ddn <- -7 to 7) yield {chessboard.filter(a => a._4 == ddn && a._5 == 'q').length <= 1}
  rows.forall(x=>x) && cols.forall(x=>x) && dups.forall(x=>x) && ddns.forall(x=>x)
}

def findEightQueens: List[List[Int]] =
  List(0,1,2,3,4,5,6,7).permutations.filter(isValid(_)).toList

val c0 = isValid(List(3,1,6,2,5,7,4,0))
val c1 = isValid(List(0,0,0,0,0,0,0,0))
val c2 = isValid(List(7,1,2,3,4,5,6,0))
val c3 = isValid(List(7,1,2,3,4,5,6,0).reverse)
val eightQueens = findEightQueens
