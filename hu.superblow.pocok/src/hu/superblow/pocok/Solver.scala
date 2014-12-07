package hu.superblow.pocok

object Solver {

  def solve() {
    for (i <- 1 to 63) {
      val weightVector = Numerics.get(i)
      val n = Numerics.n(weightVector)
      val cost = Numerics.cost(n)

      val sumCost = calculateEV(weightVector, n) + cost

      print(weightVector)
      print(" : ")
      println(sumCost)
    }
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