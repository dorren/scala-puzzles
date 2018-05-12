package number_to_words

object NumberToWords {
  val convMap:Map[Long, String] =
    Map(
       0L -> "zero",
       1L -> "one",       2L -> "two",       3L -> "three",
       4L -> "four",      5L -> "five",      6L -> "six",
       7L -> "seven",     8L -> "eight",     9L -> "nine",
      10L -> "ten",      11L -> "eleven",   12L -> "twelve",
      13L -> "thirteen", 14L -> "fourteen", 15L -> "fifteen",
      16L -> "sixteen",  17L -> "seventeen",18L -> "eighteen",
      19L -> "nineteen",
      20L -> "twenty",   30L -> "thirty",   40L -> "forty",
      50L -> "fifty",    60L -> "sixty",    70L -> "seventy",
      80L -> "eighty",   90L -> "ninety",
      100L -> "hundred",
      1000L -> "thousand",
      1000000L -> "million",
      1000000000L -> "billion")

  /**
    * convert a number to English word
    */
  def convert(num: Long, partial:Boolean = false): String = {
    num match {
      case 0 if partial         => null
      case x if x < 20L         => convMap(num)
      case x if x < 100L        => process(num, 10, partial)
      case x if x < 1000L       => process(num, 100, partial)
      case x if x < 1000000L    => process(num, 1000, partial)
      case x if x < 1000000000L => process(num, 1000000, partial)
      case x                    => process(num, 1000000000, partial)
    }
  }

  private def process(num: Long, keyNum: Long, partial: Boolean = false): String = {
    if(num < keyNum){
      convert(num, partial)
    }else {
      val remainder = num % keyNum
      val quotient_num = num - remainder

      val quotient = quotient_num / keyNum
      val quotient_word = if(keyNum < 100)
                            convMap(quotient_num)
                          else
                            convert(quotient, true) + " " + convMap(keyNum)

      Seq(quotient_word, convert(remainder, true))
        .filter(_ != null)
        .mkString(" ")
    }
  }
}
