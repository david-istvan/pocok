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
    var n = 0
    for (i <- 0 to weightVector.length - 1) {
      n = n + weightVector(i)
    }
    n
  }

  def cost(n: Int) = -2 * n + 1

  //A, B, C, D, E, F
  def matchupCoverageMatrix(n: Int) =
    List(
      List(0, .25 * n, .5, .5, .125 * n, .125 * n),
      List(.25 * n, 0, .125 * n, .125 * n, .5, .5),
      List(.5, .125 * n, 0, 1, .0625 * n, .0625 * n),
      List(.5, .125 * n, 1, 0, .0625 * n, .0625 * n),
      List(.125 * n, .5, .0625 * n, .0625 * n, 0, 1),
      List(.125 * n, .5, .0625 * n, .0625 * n, 1, 0))

  val A = new SemifinalTeam()
  val B = new SemifinalTeam()
  val C = new WildCardTeam()
  val D = new WildCardTeam()
  val E = new WildCardTeam()
  val F = new WildCardTeam()

  def EVs(n: Int) = {
    def getEVForTeam(team: Team, n: Int) = {
      team match {
        case SemifinalTeam() => (.5 + .25 * n)
        case WildCardTeam() =>
          (.5 + .25 + .125 * n)
        case _ => 0
      }
    }

    List(getEVForTeam(A, n), getEVForTeam(B, n), getEVForTeam(C, n), getEVForTeam(D, n), getEVForTeam(E, n), getEVForTeam(F, n))
  }

}

abstract class Team() {}

case class WildCardTeam() extends Team {}

case class SemifinalTeam() extends Team {}