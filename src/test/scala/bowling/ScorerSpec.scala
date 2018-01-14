package bowling

import org.scalatest.{FlatSpec, Matchers}

class ScorerSpec extends FlatSpec with Matchers {
  "Scorer" should "check strike" in {
    val actual = Seq(10)
    assert(actual === Scorer.strike)
  }

  "Scorer" should "check spare" in {
    assert(Scorer.isSpare(Seq(9, 1)))
    assert(!Scorer.isSpare(Seq(8, 1)))
  }

  "Scorer" should "get rolls for strike" in {
    val tail = Seq(Seq(10), Seq(10), Seq(1,2))
    assert(Scorer.getRolls(tail, 2) === 20)

    val tail2 = Seq(Seq(10), Seq(5, 1), Seq(1,2))
    assert(Scorer.getRolls(tail2, 2) === 15)
  }

  "Scorer" should "calculate " in {
    val game = Seq(Seq(10), Seq(10), Seq(10),
                   Seq(10), Seq(10), Seq(10),
                   Seq(10), Seq(10), Seq(10), Seq(10, 10, 10))
    assert(Scorer.calculate(game) === 300)

    val game2 = Seq(Seq(10),  Seq(7,3), Seq(1,0),
                    Seq(2,0), Seq(3,0), Seq(4,0),
                    Seq(5,0), Seq(6,0), Seq(7,0), Seq(8,0))
    // 20 + 11 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 = 67
    assert(Scorer.calculate(game2) === 67)
  }
}
