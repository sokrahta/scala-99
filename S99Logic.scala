class S99Logic(a: Boolean) {
  import S99Logic.boolean2S99Logic

  def not: Boolean = a match {
    case true  => false
    case false => true
  }
  def and (b: Boolean): Boolean = (a,b) match {
    case (true,true) => true
    case _ => false
  }
  def or  (b: Boolean): Boolean = (a,b) match {
    case (true,_) => true
    case (_,true) => true
    case _ => false
  }

  def nand(b: Boolean): Boolean = (a and b).not
  def nor (b: Boolean): Boolean = (a or a).not
  def xor (b: Boolean): Boolean = (a equ b).not
  def impl(b: Boolean): Boolean = a.not or b
  def equ (b: Boolean): Boolean = (a and b) or (a.not and b.not)
}

object S99Logic {
  implicit def boolean2S99Logic(a: Boolean): S99Logic = new S99Logic(a)

  def table2(fname: String, f: (Boolean, Boolean) => Boolean): Unit = {
    def printone(a: String, b: String, r: String) = println("%-5s %-5s %-5s = %s".format(a,fname,b,r))
    printone("A","B","result")
    for {a <- List(true,false);
         b <- List(true,false)}
      {printone(a.toString, b.toString, f(a,b).toString)}
  }

  def gray(n: Int): List[String] = {
    if (n <= 0) List("")
    else {
      val tail = gray(n-1)
      (tail map {"0"+_}) ::: (tail map {"1"+_})
    }
  }

  abstract sealed class HTree[+A] {
    val freq: Int
    def code: List[(A,String)] = code("")
    def code(prefix: String): List[(A,String)]
  }
  final case class HBlank[A]() extends HTree[A] {
    val freq: Int = 0
    def code(prefix: String) = Nil
  }
  final case class HNode[A](a: HTree[A], b: HTree[A]) extends HTree[A] {
    val freq: Int = a.freq + b.freq
    def code(prefix: String) = a.code(prefix+"0") ::: b.code(prefix+"1")
  }
  final case class HLeaf[A](a: A, freq: Int) extends HTree[A] {
    def code(prefix: String) = List((a, prefix))
  }

  def huffman[A](as: List[(A,Int)]): List[(A,String)] = {
    def build(ls: List[HTree[A]]): HTree[A] = {
      val (leafs, tail) = ls sortBy(_.freq) splitAt(2)
      leafs match {
        case a::b::Nil => build(new HNode(a,b) :: tail)
        case a::_      => a
        case _         => new HBlank
      }
    }
    val leafs: List[HTree[A]] = as map {case (a,b) => new HLeaf(a,b)}
    val tree: HTree[A] = build(leafs)
    tree.code
  }
}

object Exercises {
  import S99Logic._
  import S99Logic.boolean2S99Logic
  def run {
    table2("and",  _ and _)
    table2("or",   _ or _)
    table2("nand", _ nand _)
    table2("nor",  _ nor _)
    table2("xor",  _ xor _)
    table2("impl", _ impl _)
    table2("equ",  _ equ _)
    gray(1)
    gray(3) //res0: List[String] = List(000, 001, 011, 010, 110, 111, 101, 100)
    huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
    //res0: List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))
  }
}

Exercises.run

