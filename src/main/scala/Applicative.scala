import cats._
import cats.std.all._


object Applicativev {

  import Catnip._


  def main(args: Array[String]) {
    //produce list of function and than map them
    val hs = Functor[List].map(List(1, 2, 3, 4))(a => ((_: Int) * (_: Int)).curried(a))

    println(Functor[List].map(hs)(f => f(9)))

    //F[A] F[B]  (A, B) => C
    //ap to 22 arguments
    val v = Apply[Option].map2(none[Int], List(4, 3).some) { case (a, b) => a :: b }
    val v2 = Apply[Option].map2(3.some, List(4, 3).some) { case (a, b) => a :: b }
    val v3 = Apply[Option].map2(Some(3), Some(List(4, 3))) { case (a, b) => a :: b }
    val v4 = Apply[Option].map2(3.some, none[List[Int]]) { case (a, b) => a :: b }


    //applicative usage F[A => B] F[A]
    val vv = Applicative[List].ap(hs)(List(9, 4, 5))
    println(vv)


    val F2 = Applicative[Option]

  }
}