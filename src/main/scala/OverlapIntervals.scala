/**
  * given list of Intervals, merge adjacent intervals if they overlap. For example,
  *
  *   Seq((1,3), (2,4), (5,6)) => Seq((1,4), (5,6))
  */
object OverlapIntervals {
  type Interval = (Int, Int)

  def merge(intervals: Seq[Interval]): Seq[Interval] ={
    intervals match {
      case Nil      => intervals
      case h :: Nil => intervals
      case h :: h2 :: tail if h._2 <  h2._1 => h +: h2 +: merge(tail)
      case h :: h2 :: tail if h._2 >= h2._1 => merge((h._1, Seq(h._2, h2._2).max) +: tail)
    }
  }
}
