
object Main extends App {

  def bubbleSort[T <% Ordered[T]](data: List[T]): List[T] = data match {
    case Nil => Nil
    case _ => {
      val (fv, sl) = getLargest(data)
      bubbleSort(sl) ::: List(fv)
    }
  }

  def getLargest[T <% Ordered[T]](data: List[T]): (T, List[T]) = data match {
    case (Nil) => (null.asInstanceOf[T], Nil)
    case (fv::Nil) => (fv, Nil)
    case (fv::fl) => {
      val (fd, lso) = getLargest(fl)
      if (fd >= fv) (fd, fv :: lso) else (fv, fd :: lso)
    }
  }

  val unsorted = List(1,3,5,2,6)
  println(bubbleSort(unsorted))
}
