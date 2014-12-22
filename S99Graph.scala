abstract class GraphBase[T, U] {
  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)
    override def toString: String = value match {
      case () => "%s%s%s".format(n1.value, edgeDelim, n2.value)
      case v  => "%s%s%s%s%s".format(n1.value, edgeDelim, n2.value, labelDelim, v)
    }
  }
  case class Node(value: T) {
    var adj: List[Edge] = Nil
    // neighbors are all nodes adjacent to this node.
    def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)

    def degree: Int = adj.length

    def nodesByDepth(acc: List[Node]): List[Node] = {
      def go(acc: List[Node], n: List[Node]): List[Node] = n match {
        case Nil => Nil
        case h::t if acc.contains(h) => go(t,n)
        case h::t                    => {
          val depth = h.nodesByDepth(h::acc)
          depth ::: go(t, h::acc)
        }
      }
      go(neighbors, this :: acc) ::: List(this)
    }
  }

  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil
  
  // If the edge E connects N to another node, returns the other node,
  // otherwise returns None.
  def edgeTarget(e: Edge, n: Node): Option[Node]

  override def equals(o: Any) = o match {
    case g: GraphBase[_,_] => ((nodes.keys.toList diff g.nodes.keys.toList) == Nil &&
                               (edges.map(_.toTuple) diff g.edges.map(_.toTuple)) == Nil)
    case _ => false
  }
  def addNode(value: T) = {
    val n = new Node(value)
    nodes = Map(value -> n) ++ nodes
    n
  }

  val edgeDelim: String = ">"
  val labelDelim: String = "/"

  override def toString: String = {
    val (edgeStrs, orphans) = edges.foldLeft((Nil: List[String], nodes.values.toList))((r,e) =>
      (e.toString::r._1, r._2.filter(n => n != e.n1 && n != e.n2)))
    "[%s]".format((orphans.map(_.value.toString) ::: edgeStrs).mkString(", "))
  }

  def toTermForm: (List[T], List[(T, T, U)]) = {
    (nodes.keys.toList, edges.map(x => (x.n1.value, x.n2.value, x.value)))
  }

  def toAdjacentForm: List[(T, List[(T, U)])] = {
    nodes.keys.toList.map(x => {
      (x, edges.filter(_.n1.value == x).map(y => (y.n2.value, y.value)) )
    })
  }

  def findPaths(a: T, b: T): List[List[T]] = {
    //@annotation.tailrec
    def go(c: T, acc: List[T]): List[List[T]] = {
      if (b == c) List(c :: acc)
      else {
        val nc = nodes(c)
        nc.adj.map(edgeTarget(_,nc).get).filter(n => !acc.contains(n.value)).flatMap(n => go(n.value, c :: acc))
      }
    }
    go(a, Nil).map(_.reverse)
  }

  def findCycles(a: T): List[List[T]] = {
    val n = nodes(a)
    n.adj.flatMap(e => findPaths(edgeTarget(e,n).get.value, a)).map(a :: _).filter(_.length > 2)
  }

  def nodesByDegree: List[Node] =
    nodes.values.map(n => (n, n.degree)).toList.sortBy(-_._2).map(_._1)

  def colorNodes: List[(Node, Int)] = {
    //@annotation.tailrec
    def go(n: List[Node], acc: List[(Node, Int)], color: Int): List[(Node, Int)] = {
      if (n.isEmpty) acc
      else {
        val coloredNodes = acc.map(_._1)
        val uncoloredNodes = n.filter(r => !coloredNodes.contains(r))
        val sameColoredNodes = acc.filter(_._2==color).map(_._1)
        val sameColorTouching = sameColoredNodes.flatMap(_.neighbors).distinct
        val colorableNodes = uncoloredNodes.filter(r => !sameColorTouching.contains(r))
        colorableNodes match {
          case Nil  => go(n, acc, color+1)
          case h::t => go(n.filter(_!=h), (h,color) :: acc, color)
        }
      }
    }
    go(nodesByDegree, Nil: List[(Node,Int)], 1)
  }

  def nodesByDepthFrom(a: T): List[T] = {
    nodes(a).nodesByDepth(Nil).map(_.value)
  }
}

class Graph[T, U] extends GraphBase[T, U] {
  override val edgeDelim: String = "-"

  override def equals(o: Any) = o match {
    case g: Graph[_,_] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None

  def addEdge(n1: T, n2: T, value: U) = {
    val e = new Edge(nodes(n1), nodes(n2), value)
    edges = e :: edges
    nodes(n1).adj = e :: nodes(n1).adj
    nodes(n2).adj = e :: nodes(n2).adj
  }

  def edgeIntoGraph[T,U](e: Edge, nodes: List[Node]): Boolean =
    !(nodes.contains(e.n1) == nodes.contains(e.n2))
  def spanningTrees: List[Graph[T,U]] = {
    // @annotation.tailrec
    def go(gedges: List[Edge], gnodes: List[Node], tree: List[Edge]): List[Graph[T,U]] = {
      if (gnodes == Nil) List(Graph.termLabel(nodes.keys.toList, tree.map(_.toTuple)))
      else if (gedges == Nil) Nil
      else gedges.filter(edgeIntoGraph(_, gnodes)).flatMap({ e =>
        go(gedges.filter(_!=e), gnodes.filter(edgeTarget(e,_) == None), e :: tree)
      })
    }
    go(edges, nodes.values.toList.tail, Nil).distinct
  }
  def isTree: Boolean = spanningTrees.length == 1
  def isConnected: Boolean = spanningTrees.length >= 1

  def minimalSpanningTree(implicit f: (U) => Ordered[U]): Graph[T,U] = {
    @annotation.tailrec
    def go(gedges: List[Edge], gnodes: List[Node], tree: List[Edge]): Graph[T,U] = {
      if (gnodes == Nil) Graph.termLabel(nodes.keys.toList, tree.map(_.toTuple))
      else if (gedges == Nil) new Graph[T,U]
      else {
        val me = gedges.filter(edgeIntoGraph(_, gnodes)).sortWith(_.value < _.value).head
        go(gedges.filter(_!=me), gnodes.filter(edgeTarget(me,_) == None), me :: tree)
      }
    }
    go(edges, nodes.values.toList.tail, Nil)
  }
}

class Digraph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Digraph[_,_] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else None

  def addArc(source: T, dest: T, value: U) = {
    val e = new Edge(nodes(source), nodes(dest), value)
    edges = e :: edges
    nodes(source).adj = e :: nodes(source).adj
  }
}

abstract class GraphObjBase {
  type GraphClass[T, U]

  def addLabel[T](edges: List[(T, T)]) =
    edges.map(v => (v._1, v._2, ()))
  def term[T](nodes: List[T], edges: List[(T,T)]) =
    termLabel(nodes, addLabel(edges))
  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]): GraphClass[T, U]
  def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
    nodes.map(a => (a._1, a._2.map((_, ()))))
  def adjacent[T](nodes: List[(T, List[T])]) =
    adjacentLabel(addAdjacentLabel(nodes))
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]): GraphClass[T, U]

  val edgeDelim: String = ">"
  val labelDelim: String = "/"

  def fromString(s: String) = {//: GraphBase[Char,Unit] = {
    val s1 = s.substring(1,s.length-1).split(",").toList.map(_.trim)
    val nodes = s1.flatMap(_.split(edgeDelim)).map(_(0)).distinct
    val edges = s1.filter(_.indexOf(edgeDelim) > 0).map(x => {
      val y = x.split(edgeDelim)
      (y(0)(0), y(1)(0), ())
    })
    termLabel(nodes,edges)
  }

  def fromStringLabel(s: String) = {//: GraphBase[Char,Int] = {
    val s1 = s.substring(1,s.length-1).split(",").toList.map(_.trim)
    val nodes = s1.map(_.split(labelDelim)(0)).flatMap(_.split(edgeDelim)).map(_(0)).distinct
    val edges = s1.filter(_.indexOf(labelDelim)>0).map(x => {
      val y=x.split(labelDelim)
      y.flatMap(_.split(edgeDelim))
    }).map(z => (z(0)(0), z(1)(0), z(2).toInt))
    termLabel(nodes,edges)
  }
}

object Graph extends GraphObjBase {
  type GraphClass[T, U] = Graph[T, U]

  override val edgeDelim: String = "-"

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Graph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addEdge(v._1, v._2, v._3))
    g
  }
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
    val g = new Graph[T, U]
    for ((v, a) <- nodes) g.addNode(v)
    for ((n1, a) <- nodes; (n2, l) <- a) {
      if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
        g.addEdge(n1, n2, l)
    }
    g
  }
}

object Digraph extends GraphObjBase {
  type GraphClass[T, U] = Digraph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
    val g = new Digraph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addArc(v._1, v._2, v._3))
    g
  }
  def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
    val g = new Digraph[T, U]
    for ((n, a) <- nodes) g.addNode(n)
    for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
    g
  }
}

val termform = Graph.term(List('b', 'c', 'd', 'f', 'g', 'h', 'k'),
                          List(('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k'), ('g', 'h'))).toTermForm
val gstring = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm
//res0: (List[String], List[(String, String, Unit)]) = (List(d, k, h, c, f, g, b),List((h,g,()), (k,f,()), (f,b,()), (g,h,()), (f,c,()), (b,c,())))
val adjcform = Digraph.termLabel(List('b', 'c', 'd', 'f', 'g', 'h', 'k'),
               List(('b', 'c', 1), ('b', 'f', 2), ('c', 'f', 3), ('f', 'k', 4), ('g', 'h', 5))).toAdjacentForm
val dstring = Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm
//res1: List[(String, List[(String, Int)])] = List((m,List((q,7))), (p,List((m,5), (q,9))), (k,List()), (q,List()))
val paths1 = Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths('p', 'q')
//res0: List[List[String]] = List(List(p, q), List(p, m, q))
val paths2 = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles('f')
//res0: List[List[String]] = List(List(f, c, b, f), List(f, b, c, f))
val spans = Graph.fromString("[a-b, b-c, a-c]").spanningTrees
//res0: List[Graph[String,Unit]] = List([a-b, b-c], [a-c, b-c], [a-b, a-c])
val minspan = Graph.fromStringLabel("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree
//res0: Graph[String,Int] = [a-b/1, b-c/2]
val valency = Graph.fromString("[a-b, b-c, a-c, a-d]").nodes('a').degree
//res0: Int = 3
val degrees = Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDegree
//res1: List[Graph[String,Unit]#Node] = List(Node(a), Node(c), Node(b), Node(d))
//val colors = Graph.fromString("[a-b, b-c, a-c, a-d]").colorNodes
//res2: List[(Graph[String,Unit]#Node,Int)] = List((Node(a),1), (Node(b),2), (Node(c), 3), (Node(d), 2))
val depth = Graph.fromString("[a-b, b-c, e, a-c, a-d]").nodesByDepthFrom('d')
//res0: List[String] = List(c, b, a, d)

