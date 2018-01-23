package bowling

/**
  * calculator to calculate final score of a blowing game.
  */
object Scorer {
  type Frame = Seq[Int]

  val strike: Frame = Seq(10)

  /**
    * Check if a frame is spare
    * @param frame rolls in one frame, a Seq() of numbers.
    */
  def isSpare(frame: Frame): Boolean = {
    frame match {
      case a::b::Nil if (a + b) == 10 => true
      case _ => false
    }
  }

  /**
    * get subsequent rolls for strike or spare.
    * @param frames following frames
    * @param num, number of rolls to get
    * @return sum of rolls
    */
  def getRolls(frames:Seq[Frame], num: Int): Int ={
    frames.flatten.take(num).sum
  }

  /**
    * calculate final score for a game.
    * @param frames, frame of rolls.
    * @return final game
    */
  def calculate(frames: Seq[Frame]): Int = {
    frames match {
      case Nil    => 0
      case `strike`::tail if tail.nonEmpty  => 10 + getRolls(tail, 2) + calculate(tail)
      case frame::tail    if isSpare(frame) => 10 + getRolls(tail, 1) + calculate(tail)
      case frame::tail                      => frame.sum              + calculate(tail)
    }
  }
}
