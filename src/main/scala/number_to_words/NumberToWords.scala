package number_to_words

object NumberToWords {
  val convMap =
    Map(
       0 -> "zero",
       1 -> "one",       2 -> "two",       3 -> "three",
       4 -> "four",      5 -> "five",      6 -> "six",
       7 -> "seven",     8 -> "eight",     9 -> "nine",
      10 -> "ten",      11 -> "eleven",   12 -> "twelve",
      13 -> "thirteen", 14 -> "fourteen", 15 -> "fifteen",
      16 -> "sixteen",  17 -> "seventeen",18 -> "eighteen",
      19 -> "nineteen",
      20 -> "twenty",   30 -> "thirty",   40 -> "forty",
      50 -> "fifty",    60 -> "sixty",    70 -> "seventy",
      80 -> "eighty",   90 -> "ninety",
      100 -> "hundred",
      1000 -> "thousand",
      1000000 -> "million",
      1000000000 -> "billion")

  /**
    * convert a number to English word
    */
  def convert(num: Int, partial:Boolean = false): String = {
    num match {
      case 0 if partial     => null
      case x if x < 20      => convMap(num)
      case x if x < 100     => do2digits(x)
      case x if x < 1000    => do3digits(x)
      case x if x < 1000000 => do5digits(x)
    }
  }

  def do2digits(num: Int, partial: Boolean = false): String = {
    if(num < 10){
      convert(num, partial)
    }else {
      val remainder = num % 10 // last digit.
      val quotient_num = num - remainder // with last digit = 0

      Seq(convMap(quotient_num), convert(remainder, true))
        .filter(_ != null)
        .mkString(" ")
    }
  }

  def do3digits(num: Int, partial: Boolean = false): String = {
    if(num < 100){
      do2digits(num, partial)
    }else {
      val remainder = num % 100 // last 2 digit
      val quotient_num = num - remainder // with last 2 digits as 0

      val quotient = quotient_num / 100
      val quotient_word = convMap(quotient) + " " + convMap(100)

      Seq(quotient_word, do2digits(remainder, true))
        .filter(_ != null)
        .mkString(" ")
    }
  }

  def do5digits(num: Int, partial: Boolean = false): String = {
    if(num < 1000){
      do3digits(num, partial)
    }else {
      val remainder = num % 1000 // last 2 digit
      val quotient_num = num - remainder // with last 3 digits as 0

      val quotient = quotient_num / 1000
      val quotient_word = convert(quotient) + " " + convMap(1000)

      Seq(quotient_word, do3digits(remainder, true))
        .filter(_ != null)
        .mkString(" ")
    }
  }

  def process(num: Int, keyNum: Int, nextFn: (Int, Boolean) => String, partial: Boolean = false): String = {
    if(num < keyNum){
      nextFn(num, partial)
    }else {
      val remainder = num % keyNum
      val quotient_num = num - remainder

      val quotient = quotient_num / keyNum
      val quotient_word = convert(quotient) + " " + convMap(keyNum)

      Seq(quotient_word, nextFn(remainder, true))
        .filter(_ != null)
        .mkString(" ")
    }
  }
}
