

object Main extends App {

  def insertionSort[T](xs: List[T])(lessThan: (T, T) => Boolean): List[T] = {

    def insert(y: T, ys: List[T]): List[T] = ys match {
      case Nil => y :: Nil
      case z :: zs => 
        if (lessThan(y, z)) y :: z :: zs else z :: insert(y, zs)
    }

    xs match {
      case Nil => Nil
      case y :: ys => insert(y, insertionSort(ys)(lessThan))
    }
  }

  val nums = List(-5, 6, 3, 2, 7)
  val fruit = List("apple", "pear", "orange", "pineapple")

  println(insertionSort(nums)((a: Int, b: Int) => a < b))
  println(insertionSort(fruit)((a: String, b: String) => a.compareTo(b) < 0))
}
