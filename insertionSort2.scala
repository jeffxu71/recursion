
import scala.math.Ordering

object Main extends App {

  def insertionSort[T](xs: List[T])(ord: Ordering[T]): List[T] = {

    def insert(y: T, ys: List[T]): List[T] = ys match {
      case Nil => y :: Nil
      case z :: zs => 
        if (ord.lt(y, z)) y :: z :: zs else z :: insert(y, zs)
    }

    xs match {
      case Nil => Nil
      case y :: ys => insert(y, insertionSort(ys)(ord))
    }
  }

  val nums = List(-5, 6, 3, 2, 7)
  val fruit = List("apple", "pear", "orange", "pineapple")

  println(insertionSort(nums)(Ordering.Int))
  println(insertionSort(fruit)(Ordering.String))
}
