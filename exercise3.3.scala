
object MatchQuiz {

	sealed trait List[+A]
	case object Nil extends List[Nothing]
	case class Cons[+A](head: A, tail: List[A]) extends List[A]

	object List {
		def sum(ints: List[Int]): Int = ints match {
			case Nil => 0
			case Cons(x, xs) => x + sum(xs)
		}

		// Varadic function.
		def apply[A](as : A*): List[A] =
			if (as.isEmpty) Nil
			else Cons(as.head, apply(as.tail: _*))


		def tail(ints: List[Int]): List[Int] = ints match {
			case Nil => Nil
			case Cons(head, tail) => tail
		}

		def setHead(ints: List[Int], newHead :Int): List[Int] = ints match {
			case Nil => Nil
			case Cons(head, tail) => Cons(newHead, tail)
		}
	}

	def main(args: Array[String]) {


		val original = List(1,2,3)
		val replaced = List.setHead(original, 6)

		println("new list: %s".format(replaced))

	}

}
