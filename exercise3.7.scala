
object MatchQuiz {

	sealed trait List[+A]
	case object Nil extends List[Nothing]
	case class Cons[+A](head: A, tail: List[A]) extends List[A]

	object List {

		def apply[A](as : A*): List[A] =
			if (as.isEmpty) Nil
			else Cons(as.head, apply(as.tail: _*))

		def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
			//case Cons(z, _) => Cons(z, Nil) <-- why you no work!
			case Nil => z
			case Cons(x, xs) => f(x, List.foldRight(xs, z)(f))
		}

		def product2(ns: List[Double]) =
			List.foldRight(ns, 1.0)(_ * _)


	}

	def main(args: Array[String]) {

		val l = List(1.1, 2.2, 3.3)

		val dl = List.product2(l)
		
		println("new list: %s".format(dl))

	}

}
