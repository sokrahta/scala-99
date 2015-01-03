def isIdentifier(s: String): Boolean = {
  def isLetter(c: Char): Boolean = (c >= 'a' && c <= 'z')
  def isDigit(c: Char): Boolean = (c >= '0' && c <= '9')
  def isDash(c: Char): Boolean = c == '-'
  def isLetterDigitDash(c: Char): Boolean = isLetter(c) || isDigit(c) || isDash(c)
  @annotation.tailrec
  def go(cs: List[Char]): Boolean = cs match {
    case Nil => true
    case h::t => if (isLetterDigitDash(h)) go(t) else false
  }
  s.toLowerCase.toCharArray.toList match {
    case h::t if isLetter(h) => go(t)
    case _ => false
  }
}

val test1 = isIdentifier("goodName")
val test2 = isIdentifier("1BadName")
