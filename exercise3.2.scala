
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
	}

	def main(args: Array[String]) {

		val end = List.tail(List(3,4, 5))


		// expect something like List(4, List(5))
		println("end is %s".format(end))

	}


	// output was actually: end is Cons(4,Cons(5,Nil))

}
