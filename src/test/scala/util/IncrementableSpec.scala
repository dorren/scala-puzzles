package util

import java.time.LocalDate

import org.scalatest.{FlatSpec, Matchers}
import util.IncrementorImpl._


class IncrementableSpec extends FlatSpec with Matchers{
  "it" should "increment int" in {
    assert(1.next == 2)
  }

  "it" should "upto int" in {
    assert(1.upTo(3) == Seq(1,2,3))
  }

  "it" should "increment string" in {
    assert("a".next == "b")
    assert("z".next == "aa")
    assert("az".next == "ba")
    assert("zz".next == "aaa")
  }

  "it" should "upto String" in {
    assert("a".upTo("c") == Seq("a","b","c"))
  }

  "it" should "increment date" in {
    assert(LocalDate.of(2018, 5, 12).next == LocalDate.of(2018, 5, 13))
    assert(LocalDate.of(2018, 5, 31).next == LocalDate.of(2018, 6, 1))
  }

  "it" should "increment DateString" in {
    assert(DateString("20180514").next == DateString("20180515"))
  }

  "it" should "upTo DateString" in {
    assert(DateString("20180530").upTo(DateString("20180601")) ==
           Seq(DateString("20180530"),
               DateString("20180531"),
               DateString("20180601")))
  }

  "it" should "compare" in {
    assert(1 <= 2)
    assert("a" < "b")
    assert(LocalDate.of(2018, 5, 13) < LocalDate.of(2018, 5, 14))
    assert(DateString("20180513") < DateString("20180514"))
  }

  "it" should "work for any input type" in {
    def getNext[T](src: Incrementable[T]): T = src.next()

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
      * spark-submit --class com.corp.myApp myApp.jar \
      *   -- s3_input s3a://my-bucket/201802          \
      *   -- from 1                                   \
      *   -- to   5                                   \
      *   -- inputType Int
      */

    def main(args: Array[String]) = {
      val from      = "fromValue"  // extracted from args somehow
      val to        = "toValue"
      val inputType = "typeValue"

      val range = inputType match {
        case "DateString" => DateString(from).upTo(DateString(to))
        case "Int"        => from.toInt.upTo(to.toInt)
        case y: String    => from.upTo(to)
      }
    }
  }
}
