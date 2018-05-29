

object Main extends App {

  def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (x :: xs1, y :: ys1) =>
      if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
  }

  def mergeSort(xs: List[Int]): List[Int] = {
    val n = (xs.length / 2).toInt
    val (fst, snd) = xs splitAt n

    if (n == 0) xs else merge(mergeSort(fst), mergeSort(snd))
  }

  /*
  def genericMergeSort[T <: Ordered[T]](xs: List[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def genericMerge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case(Nil, ys) => ys
        case(xs, Nil) => xs
        case(x :: xs1, y :: ys1) =>
          if (x < y) x :: genericMerge(xs1, ys)
          else y :: genericMerge(xs, ys1)
      }
      val (left, right) = xs splitAt(n)
      genericMerge(genericMergeSort(left), genericMergeSort(right))
    }
  }
  */

  val nums = List(2, -4, 5, 7, 1)
  println(mergeSort(nums))
  //println(genericMergeSort[Int](nums))
}
