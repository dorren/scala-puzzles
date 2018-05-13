package util

import java.time.LocalDate

import org.scalatest.{FlatSpec, Matchers}

class DateStringSpec extends FlatSpec with Matchers{
  "it" should "convert to localdate" in {
    val str = "20180513"
    val expected = LocalDate.of(2018, 5, 13)

    assert(DateString(str).toLocalDate() == expected)
    assert(DateString.toString(expected) == str)
  }

  "it" should "get next day" in {
    val            day1 = DateString("20180513")
    assert(day1.next() == DateString("20180514"))
  }
}
