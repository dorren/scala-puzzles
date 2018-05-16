package util

import java.time.LocalDate
import java.util.Date

/**
  * trait for incrementing variety type of objects, number, date, String, etc.
  *
  */
trait Incrementable[T] {

  /**
    *   1.next => 2
    *   "a".next => "b"
    *   LocalDate.of(2018, 5, 13).next => LocalDate.of(2018, 5, 14)
    */
  def next(): T = ???

  /**
    *   1.upTo(3) => Seq(1,2,3)
    *   "a".upTo("c") => Seq("a", "b", "c")
    */
  def upTo(that: T)(implicit ev1: T => Incrementable[T],
                             ev2: T => Ordered[T]): Seq[T] = {
    
    var current = get()
    var result = Seq(current)

    while (current < that){        // <      is from ev2
      current = current.next()     // next() is from ev1
      result = result :+ current
    }

    result
  }

  // used in upTo(), get current [T] type value
  protected def get(): T = ???
}

/**
  * To be extended by classes that doesn't have Ordered[T] built-in.
  */
trait OrderedIncrementable[T] extends Incrementable[T] with Ordered[T]


object IncrementorImpl {
  implicit def numeric(n: Int) = {
    new Incrementable[Int] {
      override def next(): Int = n + 1

      override def get(): Int = n
    }
  }

  implicit def localdate(date: LocalDate) = {
    new OrderedIncrementable[LocalDate] {
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
    new Incrementable[String] {
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

      override def get(): String = str
    }
  }
}



