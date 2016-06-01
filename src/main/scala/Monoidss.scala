import cats._
import cats.std.all._
import cats.syntax.semigroup._
import algebra.laws.GroupLaws
import org.scalacheck.Arbitrary
import cats.syntax.foldable._

class Disjunction(val unwrap: Boolean) extends AnyVal

object Disjunction {
  @inline def apply(b: Boolean): Disjunction = new Disjunction(b)

  implicit val disjunctionMonoid: Monoid[Disjunction] = new Monoid[Disjunction] {
    def combine(a1: Disjunction, a2: Disjunction): Disjunction =
      Disjunction(a1.unwrap && a2.unwrap)

    def empty: Disjunction = Disjunction(true)
  }

  implicit val disjunctionEq: Eq[Disjunction] = new Eq[Disjunction] {
    def eqv(a1: Disjunction, a2: Disjunction): Boolean =
      a1.unwrap == a2.unwrap
  }
}


class Subtract(val i: Int) extends AnyVal {
  override def toString = i.toString
}

object Subtract {
  @inline def apply(i: Int): Subtract = new Subtract(i)

  //monoid part
  implicit val subtractFakeMonoid: Monoid[Subtract] = new Monoid[Subtract] {
    def combine(a1: Subtract, a2: Subtract): Subtract =
      Subtract(a1.i - a2.i)

    def empty: Subtract = Subtract(0)
  }

  //equality
  implicit val subtractEq: Eq[Subtract] = new Eq[Subtract] {
    def eqv(a1: Subtract, a2: Subtract): Boolean =
      a1.i == a2.i
  }
}


//usefull monoid
case class Last[A: Eq](unwrap: Option[A]) {
  override def toString = unwrap.toString
}

object Last {
  implicit def lastMonoid[A: Eq]: Monoid[Last[A]] = new Monoid[Last[A]] {
    def combine(a1: Last[A], a2: Last[A]): Last[A] =
      Last((a1.unwrap, a2.unwrap) match {
        case (_, Some(y)) => Some(y)
        case (x, None) => x
      })

    def empty: Last[A] = Last(None: Option[A])
  }

  implicit def lastEq[A: Eq]: Eq[Last[A]] = new Eq[Last[A]] {
    def eqv(a1: Last[A], a2: Last[A]): Boolean =
      Eq[Option[A]].eqv(a1.unwrap, a2.unwrap)
  }
}


object Monoids {
  def main(args: Array[String]) {

    val l = List(1, 2, 3) |+| List(4, 5, 6)
    val combined = (Some("aa"): Option[String]) |+| None

    //own implementation
    val x1 = Disjunction(true) |+| Disjunction(false)
    println(x1.unwrap)

    val dis = Monoid[Disjunction].empty |+| Disjunction(true)
    println(dis.unwrap)
    println("========================================================")

    //check new groups law
    implicit def arbConjunction(implicit ev: Arbitrary[Boolean]): Arbitrary[Disjunction] =
      Arbitrary {
        ev.arbitrary map (Disjunction(_))
      }
    GroupLaws[Disjunction].monoid.all.check
    println("========================================================")

    //check broken law
    implicit def arbSubtraction(implicit ev: Arbitrary[Int]): Arbitrary[Subtract] =
      Arbitrary {
        ev.arbitrary map (Subtract(_))
      }
    GroupLaws[Subtract].monoid.all.check
    println("========================================================")

    //get first from left that is not empty
    val n = Monoid[Last[Int]].empty
    val s = Last(Some(3))

    val vvv = n |+| n |+| s
    println(vvv)

    //fold with monoid
    val monoidsList = List(Last(Some(1)), n, Last(Some(5)))
    val folded = Foldable[List].fold(monoidsList)(Monoid[Last[Int]])
    println(folded)

    //mapp and than fold with monoid - how to use it in real world examples
    List(1, 2, 3).foldMap(identity)(Monoid.additive[Int])

    val toFold: List[Option[Int]] = List(Some(4), None, Some(0))
    val mapFolded = toFold.foldMap(Last(_))
    println(mapFolded)

    println("========================================================")

    //check if usefull monoid obey the law
    implicit def arbFirst[A: Eq](implicit ev: Arbitrary[Option[A]]): Arbitrary[Last[A]] =
      Arbitrary {
        ev.arbitrary map (Last(_))
      }
    GroupLaws[Last[Int]].monoid.all.check
    println("========================================================")

  }
}
