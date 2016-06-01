import cats._
import cats.std.all._
import cats.syntax.eq._
import cats.syntax.functor._


object Catss {


  implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.fromTruthy({
    case 0 => false
    case _ => true
  })


  implicit val intCanAppend: CanAppend[Int] = new CanAppend[Int] {
    def append(a1: Int, a2: Int): Int = a1 + a2
  }


  def main(args: Array[String]): Unit = {

    import CanTruthy.ops._
    // import!!!
    import CanAppend.ops._


    val a = 1 =!= 'a'

    Functor[List].map(List(1, 2, 3)) {
      _ + 1
    }

    val h = ((x: Int) => x + 1) map {
      _ * 7
    }

    val lifted = Functor[List].lift {
      (_: Int) * 2
    }
    lifted(List(2, 3, 4))



    //implemented
    List(1, 2, 3) fproduct {
      (_: Int) * 2
    }

    1.truthy

    println(1 |+| 2)
  }
}
