
object MatchQuiz {

	sealed trait List[+A]
	case object Nil extends List[Nothing]
	case class Cons[+A](head: A, tail: List[A]) extends List[A]

	object List {

		def apply[A](as : A*): List[A] =
			if (as.isEmpty) Nil
			else Cons(as.head, apply(as.tail: _*))

		def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
			case Nil => z
			case Cons(x, xs) => f(x, List.foldRight(xs, z)(f))
		}

	}

	def main(args: Array[String]) {

		val d = List.foldRight(List(1,2,3,4,5,6,7,8), 0)((x, y) => 1 + y)
		
		println("new list: %s".format(d))

	}

}
