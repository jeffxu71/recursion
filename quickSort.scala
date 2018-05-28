

object qSort extends App {

  def quickSort(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case a :: Nil => List(a)
    case a :: tail => quickSort(tail.filter(x => x <= a)) :::
      List(a) ::: quickSort(tail.filter(x => x > a))
  }

  val nums = List(10,30, 20, 5, 15)
  println(quickSort(nums))
}
