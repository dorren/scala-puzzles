import org.scalatest.{FlatSpec, Matchers}

class OverlapIntervalsSpec extends FlatSpec with Matchers{
  "it " should "merge basic" in {
    assert(OverlapIntervals.merge(Seq.empty) == Seq.empty)

    val src1 = Seq((1,2))
    assert(OverlapIntervals.merge(src1) == src1)  // unchanged.

    val src2 = Seq((1,2), (3,4))
    assert(OverlapIntervals.merge(src2) == src2)  // not overlapped.
  }


  "it " should "merge overlapping" in {
    val src1 = Seq((1,3), (2,4))
    assert(OverlapIntervals.merge(src1) == Seq((1,4)))

    val src2 = Seq((1,3), (2,4), (5,6))
    assert(OverlapIntervals.merge(src2) == Seq((1,4), (5,6)))
  }

  "it " should "merge edge cases" in {
    val src1 = Seq((1,10), (2,4), (3,5))
    assert(OverlapIntervals.merge(src1) == Seq((1,10)))
  }
}
