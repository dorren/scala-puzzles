package util

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import util.IncrementorImpl.localdate

/**
  * convenience class for yyyyMMdd formatted string
  *
  *   val str = "20180513"
  *
  *   DateString(str).next        => DateString("20180514")
  *   DateString(str).toString    => "20180513"
  *   DateString(str).toLocalDate => LocalDate.of(2018, 5, 13)
  */
case class DateString(str: String) extends OrderedIncrementable[DateString]{
  override def next(): DateString = {
    val day2 = toLocalDate().next()
    val day2str = DateString.toString(day2)

    DateString(day2str)
  }

  def toLocalDate(): LocalDate = {
    LocalDate.parse(str, DateTimeFormatter.BASIC_ISO_DATE) // yyyyMMdd
  }

  override def toString: String = str

  override def compare(that: DateString): Int = {
    toLocalDate().compare(that.toLocalDate())
  }
}

object DateString {
  def toString(date: LocalDate): String = {
    val pattern = DateTimeFormatter.ofPattern("yyyyMMdd")
    date.format(pattern)
  }
}
