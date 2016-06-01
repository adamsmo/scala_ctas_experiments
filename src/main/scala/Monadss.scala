import cats._, cats.std.all._, cats.syntax.flatMap._
import Catnip._


object Monadss {

  type Birds = Int

  case class Pole(left: Birds, right: Birds) {
    def landLeft(n: Birds): Option[Pole] =
      if (math.abs((left + n) - right) < 4) copy(left = left + n).some
      else none[Pole]

    def landRight(n: Birds): Option[Pole] =
      if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
      else none[Pole]
  }

  Option("a")

  val lrlr = Monad[Option].pure(Pole(0, 0)) >>= (_.landLeft(1)) >>= (_.landRight(4)) >>= (_.landLeft(-1)) >>= (_.landRight(-2))
  println(lrlr)
}
