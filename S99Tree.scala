sealed abstract class Tree[+T] {
  def isSymmetric: Boolean
  def isMirrorOf[S](other: Tree[S]): Boolean
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
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

  def fromList[A <% Ordered[A]](as: List[A]): Tree[A] = {
    def go(as: List[A], acc: Tree[A]): Tree[A] = as match {
      case Nil => acc
      case h::t => go(t, acc.addValue(h))
    }
    go(as, End)
  }
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  def isSymmetric: Boolean = left.isMirrorOf(right)
  def isMirrorOf[S](other: Tree[S]): Boolean = other match {
    case x: Node[S] => left.isMirrorOf(x.right) && right.isMirrorOf(x.left)
    case _          => false
  }
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = {
    if (value > x) Node(value, left.addValue(x), right) else Node(value, left, right.addValue(x))
  }
}

case object End extends Tree[Nothing] {
  override def toString = "."
  def isSymmetric: Boolean = true
  def isMirrorOf[S](other: Tree[S]): Boolean = {other == End}
  def addValue[U <% Ordered[U]](x: U): Tree[U] = Node(x)
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

val balanced = Tree.cBalanced(4, "x")
//res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ...
val symmetric = Node('a', Node('b'), Node('c')).isSymmetric
//res0: Boolean = true
val a = End.addValue(2)
//a: Node[Int] = T(2 . .)
val b = a.addValue(3)
//b: Node[Int] = T(2 . T(3 . .))
val c = b.addValue(0)
//res2: Node[Int] = T(2 T(0 . .) T(3 . .))
val listtree0 = Tree.fromList(List(3, 2, 5, 7, 1))
//res3: Node[Int] = T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))
val listtreesym0 = Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric
//res4: Boolean = true
val listtreesym1 = Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric
//res5: Boolean = false

