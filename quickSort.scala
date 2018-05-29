

object Main extends App {

  def quickSort(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case a :: Nil => List(a)

    // There are two equivalent ways to split the list:

    // way 1: using filter()
    //case a :: tail => quickSort(tail.filter(x => x <= a)) :::
    //  List(a) ::: quickSort(tail.filter(x => x > a))

    // way 2: using partition()
    case a :: tail => 
      val (p1, p2) = tail.partition(x => x <= a)
      quickSort(p1) ::: List(a) ::: quickSort(p2)
  }

  val nums = List(10,30, 20, 5, 15)
  println(quickSort(nums))
}
