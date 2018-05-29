import scala.annotation.tailrec

object Main extends App {

  def tcoQuickSort[T](xs: List[T])(lt: (T, T) => Boolean) = {

    @tailrec
    def loop(todo: List[List[T]], done: List[T]): List[T] = todo match {
      case Nil => done
      case xs :: rest => xs match {
        case Nil => loop(rest, done)
        case x :: xrest =>
          val (ls, rs) = (xrest partition(lt(x, _)))
          if (ls.isEmpty) {
            if (rs.isEmpty) loop(rest, x :: done)
            else loop(rs :: rest, x :: done)
          }
          else loop (ls :: List(x) :: rs :: rest, done)
      }
    }
    loop(List(xs), Nil)
  }

  val l = List[Int](6,12,3,9,3,5,1)
  val z = tcoQuickSort(l)((x, y) => if (x<y) true else false)

  println(z)
}
