sealed abstract class Tree[+T] {
  def isSymmetric: Boolean
  def isMirrorOf[S](other: Tree[S]): Boolean
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
  def nodeCount: Int
  def leafCount: Int
  def leafList: List[T]
  def internalList: List[T]
  def atLevel(n: Int): List[T]
  def layoutBinaryTree: Tree[T] = layoutBinaryTreeInt(1, 1)._1
  def layoutBinaryTreeInt(x: Int, y: Int): (Tree[T], Int)
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

  def symmetricBalancedTrees[A](n: Int, a: A): List[Tree[A]] =
    cBalanced(n, a) filter {_.isSymmetric}

  def hbalTrees[A](h: Int, a: A): List[Tree[A]] = h match {
    case h if h < 1 => List(End)
    case 1          => List(Node(a))
    case _          => {
      val big = hbalTrees(h-1, a)
      val lil = hbalTrees(h-2, a)
      big.flatMap(x => big.map(y => Node(a, x, y))) :::
      big.flatMap(x => lil.flatMap(y => List(Node(a, x, y), Node(a, y, x)) ))
    }
  }

  def minHbalNodes(h: Int): Int = h match {
    case h if h < 3 => Math.max(0,h)
    case h          => minHbalNodes(h-1)+minHbalNodes(h-2)+1
  }
  def maxHbalNodes(h: Int): Int = 2*h-1

  def minHbalHeight(n: Int): Int =
    if (n <= 0) 0
    else minHbalHeight(n / 2) + 1
  def maxHbalHeight(n: Int): Int =
    Stream.from(1).takeWhile(minHbalNodes(_) <= n).last

  def hbalTreesWithNodes[A](n: Int, a: A): List[Tree[A]] =
    (minHbalHeight(n) to maxHbalHeight(n)).flatMap(h => hbalTrees(h, a)).filter(_.nodeCount == n).toList

  def completeBinaryTree[A](n: Int, a: A): Tree[A] = n match {
    case n if n <= 0 => End
    case n if n == 1 => Node(a)
    case n if n%2==0 => Node(a, completeBinaryTree((n-1)/2+1,a), completeBinaryTree((n-1)/2,a))
    case n           => Node(a, completeBinaryTree((n-1)/2,a),   completeBinaryTree((n-1)/2,a))
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
  def nodeCount: Int = 1 + left.nodeCount + right.nodeCount
  def leafCount: Int =
    if ((left,right) == (End,End)) 1
    else left.leafCount+right.leafCount
  def leafList: List[T] =
    if ((left,right) == (End,End)) List(value)
    else left.leafList ::: right.leafList
  def internalList: List[T] =
    if ((left,right) == (End,End)) Nil
    else value :: left.internalList ::: right.internalList
  def atLevel(n: Int): List[T] = {
    if (n<1) Nil
    else if (n==1) List(value)
    else left.atLevel(n-1) ::: right.atLevel(n-1)
  }
  def layoutBinaryTreeInt(x: Int, y: Int): (Tree[T], Int) = {
    val (l, lx) = left.layoutBinaryTreeInt(x, y+1)
    val (r, rx) = right.layoutBinaryTreeInt(lx+1, y+1)
    (PositionedNode(value, l, r, lx, y), rx)
  }
}

case object End extends Tree[Nothing] {
  override def toString = "."
  def isSymmetric: Boolean = true
  def isMirrorOf[S](other: Tree[S]): Boolean = {other == End}
  def addValue[U <% Ordered[U]](x: U): Tree[U] = Node(x)
  def nodeCount = 0
  def leafCount = 0
  def leafList = Nil
  def internalList = Nil
  def atLevel(n: Int) = Nil
  def layoutBinaryTreeInt(x: Int, y: Int) = (End, y)
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

//TODO: fix case-to-case inheritance
case class PositionedNode[+T](
        override val value: T, 
        override val left: Tree[T], 
        override val right: Tree[T], 
        x: Int, y: Int) 
        extends Node[T](value, left, right) {
  override def toString = s"T[%s,%s](%s %s %s)".format(x,y,value,left,right)
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
val symtree = Tree.symmetricBalancedTrees(5, "x")
//res0: List[Node[String]] = List(T(x T(x . T(x . .)) T(x T(x . .) .)), T(x T(x T(x . .) .) T(x . T(x . .))))
val hbaltrees = Tree.hbalTrees(3, "x")
//res0: List[Node[String]] = List(T(x T(x T(x . .) T(x . .)) T(x T(x . .) T(x . .))), T(x T(x T(x . .) T(x . .)) T(x T(x . .) .)), ...
val hbalMinN = Tree.minHbalNodes(3)
//res0: Int = 4
val hbalMaxH = Tree.maxHbalHeight(4)
//res1: Int = 3
val hbaltrees2 = Tree.hbalTreesWithNodes(4, "x")
//res2: List[Node[String]] = List(T(x T(x T(x . .) .) T(x . .)), T(x T(x . T(x . .)) T(x . .)), ...
val leafcount = Node('x', Node('x'), End).leafCount
//res0: Int = 1
val leaflist = Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList
//res0: List[Char] = List(b, d, e)
val internalnodelist = Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList
//res0: List[Char] = List(a, c)
val nodecount = Node('x', Node('x'), End).nodeCount
//res0: Int = 2
val levellist = Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2)
//res0: List[Char] = List(b, c)
val cbtree = Tree.completeBinaryTree(6, "x")
//res0: Node[String] = T(x T(x T(x . .) T(x . .)) T(x T(x . .) .))
val pbtree = Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree
//res0: PositionedNode[Char] = T[3,1](a T[1,2](b . T[2,3](c . .)) T[4,2](d . .))
