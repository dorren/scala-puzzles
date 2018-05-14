package util

import java.time.LocalDate
import java.util.Date

/**
  * trait for incrementing variety type of objects, number, date, char, etc.
  *
  */
trait Incrementor[T] {
  def next(): T = ???
}

trait OrderedIncrementor[T] extends Incrementor[T] with Ordered[T]


object IncrementorImpl {
  implicit def numeric(n: Int) = {
    new Incrementor[Int] {
      override def next(): Int = n + 1
    }
  }

  implicit def localdate(date: LocalDate) = {
    new OrderedIncrementor[LocalDate] {
      override def next(): LocalDate = {
        date.plusDays(1)
      }
      override def compare(that: LocalDate): Int = {
        if(date.isBefore(that))
          -1
        else if(date.isAfter(that))
          1
        else
          0
      }
    }
  }

  implicit def string(str: String) = {
    new Incrementor[String] {
      /**
        * increment string.
        *   "a" => "b"
        *   "z" => "aa"
        *   "aaz" => "aba"
        */
      override def next(): String = {
        def incrementString(s: String): String = {
          if(s.isEmpty)
            return "a"

          val (head, tail) = s.splitAt(s.length - 1)  // split off last char
          val tail_char = tail.charAt(0)

          tail_char match {
            case 'z' => incrementString(head) + 'a'
            case  _  => head + (tail_char + 1).toChar
          }
        }

        incrementString(str)
      }
    }
  }
}



