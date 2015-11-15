type Point = (Int,Int)
type Moves = Seq[Point]

case class Branch(p: Point, tour: Moves, board: Moves) {}

case class Knight() {
  private val minimumBoard: Int = 4

  def moves(p: Point, board: Moves): Moves = {
    val (x,y) = p
    Seq( (1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2) )
      .map { case (dx,dy) => (x+dx,y+dy) }
      .filter { case (x2,y2) => board.contains((x2,y2)) }
  }

  def tours(p: Point, n: Int): Seq[Moves] = {
    def doMoves(b: Branch): Seq[Branch] = b match { case Branch(p1, tour, board) =>
      if (board.isEmpty) {
        Seq(Branch(p1, tour, board))
      } else {
        moves(p1, board).map {
          case p2 => Branch(p2, tour :+ p2, board.filterNot(_ == p2))
        }
      }
    }

    @annotation.tailrec
    def go(branches: Seq[Branch]): Seq[Branch] = {
      if (branches.contains((b: Branch) => b.board.nonEmpty)) go(branches.flatMap(doMoves)) else Seq.empty
    }

    if (n < minimumBoard)
      throw new IllegalArgumentException(s"Knight cannot tour a board smaller than $minimumBoard x $minimumBoard")

    val ns = List.range(0, n)
    val initialBoard = ns.flatMap(i => ns.map(j => (i,j)))
      .filterNot { _ == p }
    val initialTour = Seq(p)
    go(Seq(Branch(p, initialTour, initialBoard)))
      //.map { case (p, tour, board) if board.isEmpty => tour }
  }

}

(Knight()).tours((0,1), 4).foreach { l => println(l) }
