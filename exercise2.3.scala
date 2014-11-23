
object currier {

	def curry[A,B,C](a: A, f: (A,B) => C): (B => C) = (b) => f(a, b)

	def main(args: Array[String]) = {
		val curried_add = curry(2.1, (x :Double,y :Double) => x + y)
		val result :Double = curried_add(3.0)
		println("> result: %s".format(result))
	}

}
