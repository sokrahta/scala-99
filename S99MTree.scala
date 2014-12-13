case class MTree[+T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List())
  def toString1 = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
  override def toString = s"%s%s^".format(value, children.map(_.toString).mkString(""))
  def nodeCount: Int = 1 + children.map(_.nodeCount).sum
}

object MTree {
  def apply[T](value: T) = new MTree(value, List())
  //apply is provided by case class ctor
  //def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)
}

val example = MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e')))))
val ncount = MTree('a', List(MTree('f'))).nodeCount
//res0: Int = 2
val mtser = MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString
//res0: String = afg^^c^bd^e^^^
