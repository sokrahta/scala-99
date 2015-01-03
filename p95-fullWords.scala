def fullWords(n: Int): String = {
  @annotation.tailrec
  def go(n: List[Int], acc: List[String]): String = n match {
    case Nil => acc.reverse.mkString("-")
    case h::t => go(t, intToWord(h) :: acc)
  }
  go(decimalList(n,Nil),Nil)
}
@annotation.tailrec
def decimalList(n: Int, acc: List[Int]): List[Int] =
  if (n <= 0) acc else decimalList(n/10, n%10 :: acc)
def intToWord(n: Int): String = n match {
  case 0 => "zero"
  case 1 => "one"
  case 2 => "two"
  case 3 => "three"
  case 4 => "four"
  case 5 => "five"
  case 6 => "six"
  case 7 => "seven"
  case 8 => "eight"
  case 9 => "nine"
  case _ => throw new Exception("out of range")
}
  

val oneseventyfive = fullWords(175)
