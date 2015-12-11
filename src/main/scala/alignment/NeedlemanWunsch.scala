package alignment

import scala.collection.mutable.{ArrayBuffer, Queue => MutableQueue, Stack => MutableStack}

/**
  * Directions for NeedlemanWunsch
  */
object NWDirection {
  val NORTH = 0
  val WEST = 1
  val NORTHWEST = 2
}


/**
  * Dynamic programming approach of NeedlemanWunsch of helper
  * @param score The ceil score
  */
case class NWCeil(var score: Double = 0) {
  val tracebacks = MutableQueue[Int]()
}

/**
  * Needleman Wunsch algotihm
  * @param string1 The first input string
  * @param string2 The second input string
  * @param scores (match,mismatch,gap) scores
  */
class NeedlemanWunsch(string1: String, string2: String, scores: (Double, Double, Double)) {
  val (matchScore, mismatchScore, gapScore) = scores
  val dp = prepare_matrix(string1.length + 1, string2.length + 1)

  /**
    * @param onlyOne return only one alignment
    * @return Map object of alignments
    */
  def nw(onlyOne: Boolean = false): Map[String, Any] = {
    for (i <- 1 to string1.length; j <- 1 to string2.length) {
      val north = dp(i - 1)(j).score + gapScore
      val west = dp(i)(j - 1).score + gapScore
      val northwest = if (string1(i - 1) == string2(j - 1)) {
        dp(i - 1)(j - 1).score + matchScore
      } else dp(i - 1)(j - 1).score + mismatchScore

      /**
        * max : (Score, Array[(NWDirection, Score)]
        */
      val max: (Double, Array[(Int, Double)]) = Array(
        (NWDirection.NORTH, north),
        (NWDirection.WEST, west),
        (NWDirection.NORTHWEST, northwest)
      ).groupBy(_._2).maxBy(_._1)

      dp(i)(j).score = max._1
      dp(i)(j).tracebacks ++= max._2.map(_._1)
    }
    Map("nw" -> traceback(onlyOne), "score" -> dp.last.last.score)
  }

  /**
    * @param row Matrix's row
    * @param column Matrix's column
    * @return Matrix for dynamic programming
    */
  private def prepare_matrix(row: Int, column: Int): Array[Array[NWCeil]] = {
    val arr = Array.fill[NWCeil](row, column)(NWCeil())
    arr(0).indices.foreach(i => (arr(0)(i).score = i * gapScore, arr(0)(i).tracebacks += NWDirection.WEST))
    arr.indices.foreach(i => (arr(i)(0).score = i * gapScore, arr(i)(0).tracebacks += NWDirection.NORTH))
    arr
  }

  /**
    * @param onlyOne return only one alignment
    * @return sequence of alignments
    */
  private def traceback(onlyOne: Boolean): ArrayBuffer[(String, String)] = {
    val tracebackStack = MutableStack[(String, String, Int, Int)](("", "", string1.length, string2.length))
    val result = ArrayBuffer[(String, String)]()
    var condition = true
    while (condition) {
      var (s1, s2, i, j) = tracebackStack.pop()
      while (i > 0 || j > 0) {
        {
          if (dp(i)(j).tracebacks.size > 1) {
            tracebackStack.push((s1, s2, i, j))
            dp(i)(j).tracebacks.dequeue
          } else {
            dp(i)(j).tracebacks.head
          }
        } match {
          case NWDirection.NORTH =>
            s2 += "-"
            i -= 1
            s1 += string1(i)
          case NWDirection.WEST =>
            j -= 1
            s2 += string2(j)
            s1 += "-"
          case NWDirection.NORTHWEST =>
            j -= 1
            s2 += string2(j)
            i -= 1
            s1 += string1(i)
        }
      }
      result += ((s1.reverse, s2.reverse))
      if (onlyOne || tracebackStack.isEmpty) {
        condition = false
      }
    }
    result
  }
}