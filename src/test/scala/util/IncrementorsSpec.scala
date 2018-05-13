package util

import java.time.LocalDate

import org.scalatest.{FlatSpec, Matchers}
import util.IncrementorImpl._


class IncrementorsSpec extends FlatSpec with Matchers{
  "it" should "increment int" in {
    assert(1.next == 2)
  }

  "it" should "increment date" in {
    assert(LocalDate.of(2018, 5, 12).next == LocalDate.of(2018, 5, 13))
    assert(LocalDate.of(2018, 5, 31).next == LocalDate.of(2018, 6, 1))
  }

  "it" should "increment string" in {
    assert("a".next == "b")
    assert("z".next == "aa")
    assert("az".next == "ba")
    assert("zz".next == "aaa")
  }

  "it" should "work for any input type" in {
    def getNext[T](src: Incrementor[T]): T = src.next()

    assert(getNext(1) == 2)
    assert(getNext("a") == "b")

    val day1 = LocalDate.of(2018, 5, 12)
    assert(getNext(day1) == LocalDate.of(2018, 5, 13))

    val dateStr = DateString("20180512")
    assert(getNext(dateStr) == DateString("20180513"))
  }

  "it" should "work for CLI app" in {
    /**
      * spark-submit --class com.corp.myApp myApp.jar \
      *   -- s3_input s3a://my-bucket/folder1         \
      *   -- from 20180512                            \
      *   -- to   20180530                            \
      *   -- inputType DateString
      *
      *
      *   spark-submit --class com.corp.myApp myApp.jar \
      *   -- s3_input s3a://my-bucket/201802            \
      *   -- from 1                                     \
      *   -- to   5                                     \
      *   -- inputType Int
      */

  }
}