sealed abstract class Tree[+T] {
  def isSymmetric: Boolean
  def isMirrorOf[S](other: Tree[S]): Boolean
}

object Tree {
  def cBalanced[A](n: Int, a: A): List[Tree[A]] = n match {
    case n if n < 1      => List(End)
    case n if n % 2 == 1 => {
      val subtree = cBalanced(n/2, a)
      subtree.flatMap(l => subtree.map(r => Node(a,l,r) ) )
    }
    case n               => {
      val subtreeL = cBalanced((n-1)/2+1, a)
      val subtreeR = cBalanced((n-1)/2, a)
      subtreeL.flatMap(l => subtreeR.flatMap(r => List(Node(a,l,r), Node(a,r,l)) ) )
    }
  }
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  def isSymmetric: Boolean = left.isMirrorOf(right)
  def isMirrorOf[S](other: Tree[S]): Boolean = other match {
    case x: Node[S] => left.isMirrorOf(x.right) && right.isMirrorOf(x.left)
    case _          => false
  }
}

case object End extends Tree[Nothing] {
  override def toString = "."
  def isSymmetric: Boolean = true
  def isMirrorOf[S](other: Tree[S]): Boolean = {other == End}
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

Tree.cBalanced(4, "x")
//res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ...
Node('a', Node('b'), Node('c')).isSymmetric
//res0: Boolean = true

