
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

		def drop[A](l: List[A], n: Int): List[A] = l match {
			case Nil => Nil
			case Cons(x, y) => if (n > 0) drop(y, n - 1) else Cons(x, y)
		}

		def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
			case Nil => Nil
			case Cons(x, y) =>
				if (f(x))
					dropWhile(y, f)  // continue recursion without x
				else
					Cons(x, dropWhile(y, f)) //keep x, recurse without it, continue with y.
		}

	}

	def main(args: Array[String]) {

		val l = List(1,2,3,4,5,6,7,8)

		// drop even numbers!
		val dl = List.dropWhile(l, (a: Int) => (a % 2 == 0))
		
		println("new list: %s".format(dl))

	}

}
