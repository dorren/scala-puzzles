package number_to_words

import org.scalatest.{FlatSpec, Matchers}

class NumberToWordsSpec extends FlatSpec with Matchers{
  "it" should "convert single digit" in {
    assert(NumberToWords.convert(0) == "zero")
    assert(NumberToWords.convert(1) == "one")
    assert(NumberToWords.convert(9) == "nine")
  }

  "it" should "convert teens" in {
    assert(NumberToWords.convert(11) == "eleven")
    assert(NumberToWords.convert(19) == "nineteen")
  }

  "it" should "convert 2 digits" in {
    assert(NumberToWords.convert(20) == "twenty")
    assert(NumberToWords.convert(31) == "thirty one")
  }

  "it" should "convert 3 digits" in {
    assert(NumberToWords.convert(100) == "one hundred")
    assert(NumberToWords.convert(234) == "two hundred thirty four")
  }

  "it" should "convert 4-6 digits" in {
    assert(NumberToWords.convert(1000)   == "one thousand")
    assert(NumberToWords.convert(2003)   == "two thousand three")
    assert(NumberToWords.convert(34567)  == "thirty four thousand five hundred sixty seven")
    assert(NumberToWords.convert(300000) == "three hundred thousand")
    assert(NumberToWords.convert(304000) == "three hundred four thousand")
  }

  "it" should "convert 6-9 digits" in {
    assert(NumberToWords.convert(1000000) == "one million")
    assert(NumberToWords.convert(2300000) == "two million three hundred thousand")
  }

  "it" should "convert up to 10-12 digits" in {
    assert(NumberToWords.convert(1000000000) == "one billion")
    assert(NumberToWords.convert(1002000000) == "one billion two million")
  }
}
