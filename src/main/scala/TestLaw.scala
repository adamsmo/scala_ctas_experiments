import cats._
import cats.laws.discipline.FunctorTests
import cats.std.all._
import org.scalacheck.{Arbitrary, Gen}

sealed trait COption[+A]

case class CSome[A](counter: Int, a: A) extends COption[A]

case object CNone extends COption[Nothing]

object COption {
  //equality for cats
  implicit def coptionEq[A]: Eq[COption[A]] = new Eq[COption[A]] {
    def eqv(a1: COption[A], a2: COption[A]): Boolean = a1 == a2
  }
}


object Usage {

  import cats.syntax.functor._
  import cats.syntax.eq._

  def main(args: Array[String]) {

    implicit val cOptionToFunctor = new Functor[COption] {
      def map[A, B](fa: COption[A])(f: A => B): COption[B] =
        fa match {
          case CNone => CNone
          case CSome(c, a) => CSome(c + 1, f(a))
        }
    }


    val v = Functor[COption].map(CSome(3, "ala"))(_ + "ss")


    (CSome(32, "aa"): COption[String]) map (_ + "ew")

    (CSome(32, "aa"): COption[String]) === CSome(32, "")

    (CSome(32, "aa"): COption[String]) === CNone

    //(CSome(32, "aa"): COption[String]) === (CSome(32, 23.3): COption[Double])

    //
    //    CSome(4,21) === CSome(4,12)
  }
}


object TestLaw {
  def main(args: Array[String]) {

    FunctorTests[Either[Int, ?]].functor[Int, Int, Int].all.check

    FunctorTests[List].functor[Int, Int, Int].all.check

    FunctorTests[Option].functor[Int, Int, Int].all.check

    //generate arbitrary values for testing
    implicit def coptionArbiterary[A](implicit arbA: Arbitrary[A]): Arbitrary[COption[A]] =
      Arbitrary {
        val arbSome = for {
          i <- implicitly[Arbitrary[Int]].arbitrary
          a <- arbA.arbitrary
        } yield CSome(i, a): COption[A]
        val arbNone = Gen.const(CNone: COption[Nothing])
        Gen.oneOf(arbSome, arbNone)
      }

    //define functor bease on your container
    implicit val coptionFunctor = new Functor[COption] {
      def map[A, B](fa: COption[A])(f: A => B): COption[B] =
        fa match {
          case CNone => CNone
          case CSome(c, a) => CSome(c + 1, f(a))
        }
    }

    FunctorTests[COption].functor[Int, Int, Int].all.check

  }
}
