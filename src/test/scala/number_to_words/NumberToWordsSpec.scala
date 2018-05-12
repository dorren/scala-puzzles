package number_to_words

import org.scalatest.{FlatSpec, Matchers}

class NumberToWordsSpec extends FlatSpec with Matchers{
  "it" should "convert single digit" in {
    assert("zero" == NumberToWords.convert(0))
    assert("one"  == NumberToWords.convert(1))
    assert("nine" == NumberToWords.convert(9))
  }

  "it" should "convert teens" in {
    assert("eleven"  == NumberToWords.convert(11))
    assert("nineteen" == NumberToWords.convert(19))
  }

  "it" should "convert 2 digits" in {
    assert("twenty"  == NumberToWords.convert(20))
    assert("thirty one" == NumberToWords.convert(31))
  }

  "it" should "convert 3 digits" in {
    assert("one hundred"  == NumberToWords.convert(100))
    assert("two hundred thirty four" == NumberToWords.convert(234))
  }

  "it" should "convert 5 digits" in {
    assert("one thousand"  == NumberToWords.convert(1000))
    assert("two thousand three" == NumberToWords.convert(2003))
    assert("thirty four thousand five hundred sixty seven" == NumberToWords.convert(34567))
  }
}
