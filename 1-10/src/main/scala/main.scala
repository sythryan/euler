import scala.annotation._

object main extends App {
  // #1 
  // If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  // Find the sum of all the multiples of 3 or 5 below 1000.

  def findAllMultiples(multiple: Int, under: Int): List[Int] = {
    @tailrec
    def inner(nextMultiple: Int, acc: List[Int]): List[Int] = nextMultiple match {
      case e if (e < 1000) => nextMultiple match {
        case e if (e % multiple == 0) => inner(nextMultiple + 1, nextMultiple +: acc)
        case _ => inner(nextMultiple + 1, acc)
      }
      case _ => acc
    }
    inner(1, List())
  }
  println(((findAllMultiples(5, 1000) ++ findAllMultiples(3, 1000)).distinct).sum)
}