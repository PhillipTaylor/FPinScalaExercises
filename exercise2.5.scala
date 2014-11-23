
object Composer {

	def compose[A,B,C](f: B => C, g: A => B): A => C = (a :A) => f(g(a))

	def times_three(x :Int) :Int = (x * 3)
	def minus_three(y :Int) :Int = (y - 3)

	def main(args: Array[String]) {
		val composed_function = compose(times_three, minus_three)
		val result = composed_function(22)
		println("got this: %s".format(result))
	}

	// answer: 57. 22 * 3 = 60, - 3 = 57 :-)

}
