/**
  * check if a word's letters can all be substituted with Periodic table element symbol.
  *
  * For example,
  *   "bank" => Seq("ba", "n", "k")
  *   "bat"  => Seq("b", "at")
  *   "ryan" => // not possible, can not match all letters.
  *
  *   https://en.wikipedia.org/wiki/List_of_chemical_elements
  */
object ElementWord {
  private val table = Elements.table
  type Solution = Seq[String]

  def hasSymbol(symbol: String): Boolean = {
    table.exists(_._1 == symbol.capitalize)
  }

  /**
    * convert element symbol to name.
    *   "h"  => "(H)elium"
    *   "Si" => "(Si)licon"
    */
  def getElement(symbol: String): String = {
    if(hasSymbol(symbol)) {
      val capSym = symbol.capitalize  // capitalize 1st letter
      val name = table(capSym)
      s"($capSym) $name"
    }else{
      null
    }
  }

  /**
    * split off first 1 or 2 letters of the word, then look up the head symbol
    * and solve the tail.
    */
  def splitWord(word: String, index: Int = 1): Seq[Solution] = {
    val (head, tail) = word.splitAt(index)
    val head_sym = getElement(head)

    if(head_sym == null){
      Seq.empty
    }else {
      solve(tail).map(t => head_sym +: t)
    }
  }

  /**
    * main method, return all solutions for a given word
    */
  def solve(word: String): Seq[Solution] = {
    word.length match {
      case 0 => Seq(Seq.empty)
      case 1 => splitWord(word)
      case n => splitWord(word) ++ splitWord(word, 2)
    }
  }
}
