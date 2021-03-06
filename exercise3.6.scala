
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

		def init[A](l: List[A]): List[A] = l match {
			case Nil => Nil
			case Cons(x, Nil) => Nil              // cause x to be dropped
			case Cons(x, y) => Cons(x, init(y))   // keep x, call to generate tail
		}

	}

	def main(args: Array[String]) {

		val l = List(1,2,3,4,5,6,7,8)

		val dl = List.init(l) // lose 8
		
		println("new list: %s".format(dl))

	}

}
