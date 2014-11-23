
object SortChecker {

	def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
	
		@annotation.tailrec
		def compareVals(as: Array[A], ordered: (A,A) => Boolean, p: Int): Boolean = {
			if (p == as.length) //end of list. must be ok.
				true
			else {
				val x: A = as(p-1)
				val y: A = as(p)
				if (!(ordered(x,y))) // call func ptr to see if ordered.
					false
				else
					compareVals(as, ordered, p+1) //recursive tail call
			}
		}

		compareVals(as, ordered, 1)
	}

	def main(args: Array[String]) {

		val sortedInts = isSorted(Array(1,2,3,4 ), (x :Int,y :Int) => x <= y)
		val sortedStrings = isSorted(Array("ab", "cd", "ef", "gh", "hi"), (x :String,y :String) => x <= y)
		val unsortedInts = isSorted(Array(1,2,4,3), (x :Int,y :Int) => x <= y)
		val dupeInts = isSorted(Array(1,2,4,4), (x :Int,y :Int) => x <= y)

		println("expect true for Int: %s".format(sortedInts))
		println("expect true for String: %s".format(sortedStrings))
		println("expect false for Int: %s".format(unsortedInts))
		println("expect true for dupe Ints: %s".format(dupeInts))

		println("hello, world!")
	}
}
