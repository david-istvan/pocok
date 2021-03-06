package hu.superblow.pocok

import com.sun.org.apache.bcel.internal.generic.RET

object Solver {

  def solve() {
    weightVectors.groupBy(vector => {
      val n = Numerics.n(vector)
      val cost = Numerics.cost(n)
      calculateEV(vector, n) + cost
    })
      .toSeq
      .sortBy(-_._1)
      .foreach(entry => {
        println(" * " + entry._1)
        entry._2.foreach(vectorEntry => {
          println("  * " + vectorEntry.toString)
        })
      })
  }

  val weightVectors = {
    def vectorize(current: Int, to: Int, acc: List[List[Int]]): List[List[Int]] = {
      val weightVector = Numerics.get(current)
      val newVector = acc ::: List(weightVector)
      if (current < to) {
        vectorize(current + 1, to, newVector)
      } else {
        newVector
      }
    }

    vectorize(1, 63, List())
  }

  def calculateEV(weightVector: List[Int], n: Int): Double = {
    def sumRow(row: List[Double]) = {
      row.foldLeft(0.0)(_ + _)
    }

    if (n == 1) {
      val index = weightVector.indexOf(1)
      return Numerics.EVs(n)(index)
    } else {
      var sumEVs = 0.0
      for (i <- 0 to weightVector.length - 1) {
        if (weightVector(i) == 1) {
          val rowI = Numerics.matchupCoverageMatrix(n)(i)
          sumEVs = sumEVs + sumRow(rowI)
        }
      }
      sumEVs / 2
    }
  }
}