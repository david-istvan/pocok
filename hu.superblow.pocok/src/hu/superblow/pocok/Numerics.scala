package hu.superblow.pocok

object Numerics {

  def get(weightCode: Int): List[Int] = {
    val binary = weightCode.toBinaryString.reverse.padTo(6, "0").reverse

    List(
      binary(0).toString.toInt,
      binary(1).toString.toInt,
      binary(2).toString.toInt,
      binary(3).toString.toInt,
      binary(4).toString.toInt,
      binary(5).toString.toInt)
  }

  def n(weightVector: List[Int]) = {
    weightVector.foldLeft(0)(_ + _)
  }

  def cost(n: Int) = -2 * n + 1

  //s1, s2, w11, w12, w21, w22
  def matchupCoverageMatrix(n: Int) =
    List(
      List(0, .25 * n, .5, .5, .125 * n, .125 * n),
      List(.25 * n, 0, .125 * n, .125 * n, .5, .5),
      List(.5, .125 * n, 0, 1, .0625 * n, .0625 * n),
      List(.5, .125 * n, 1, 0, .0625 * n, .0625 * n),
      List(.125 * n, .5, .0625 * n, .0625 * n, 0, 1),
      List(.125 * n, .5, .0625 * n, .0625 * n, 1, 0))

  def EVs(n: Int) = {
    def semifinal(n: Int) = .5 + .25 * n
    def wildcard(n: Int) = .5 + .25 + .125 * n

    List(semifinal(n), semifinal(n), wildcard(n), wildcard(n), wildcard(n), wildcard(n))
  }

}