case class MTree[+T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List())
  def toString1 = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
  override def toString = s"%s%s^".format(value, children.map(_.toString).mkString(""))
  def nodeCount: Int = 1 + children.map(_.nodeCount).sum
  def internalPathLength: Int =
    children.foldLeft(0)((m,a) => m + a.nodeCount + a.internalPathLength)
}

object MTree {
  def apply[T](value: T) = new MTree(value, List())
  //apply is provided by case class ctor
  //def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)

  implicit def string2MTree(ns: String): MTree[Char] = {
    @annotation.tailrec
    def runtot(xs: List[(Char,Int)], t: Int, acc: List[(Char,Int)]): List[(Char,Int)] = xs match {
      case Nil => acc.reverse
      case (x,d) :: tail => runtot(tail, t+d, (x,t+d)::acc)
    }
    @annotation.tailrec
    def children(xs: List[(Char,Int)], acc: List[List[Char]]): List[List[Char]] = {
      if (xs == Nil) acc.reverse
      else {
        val i = xs.indexWhere(_._2 == 1)
        val (l,r) = xs.splitAt(i+1)
        children(r, l.map(_._1)::acc)
      }
    }
    val remainder = runtot(ns.toList.map(a => (a, if (a=='^') -1 else 1) ), 0, Nil).drop(1).filter(_._2 > 0)
    MTree(ns(0), children(remainder, Nil).map(as => string2MTree(as.mkString(""))))
  }
}

val example = MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e')))))
val ncount = MTree('a', List(MTree('f'))).nodeCount
//res0: Int = 2
val mtser = MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString
//res0: String = afg^^c^bd^e^^^
val mtdes = MTree.string2MTree("afg^^c^bd^e^^^")
import MTree.string2MTree
val inpath = "afg^^c^bd^e^^^".internalPathLength
//res0: Int = 9
