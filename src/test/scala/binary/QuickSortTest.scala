package binary

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class QuickSortTest extends AnyWordSpec with Matchers {

  "Quick sort algorithm" should {
    val quickSortAlgorithm = new QuickSortExample()
    "correctly sort strings" in {
      val controlSortedList = List("aabc", "aabd", "abbc", "bar", "foo", "lsap")
      val unsortedList = List("aabd", "foo", "bar", "abbc", "lsap", "aabc")
      val sortedList = quickSortAlgorithm.quickSort(unsortedList)
      sortedList should === (controlSortedList)
    }

    "correctly sort list with doubles" in {
      val controlSortedList = List(7.5, 9.6, 10.4, 16.5, 19.6, 20.2, 24.4, 27.6, 30.9, 32.0, 32.1)
      val unsortedList = List(27.6, 24.4, 20.2, 32.1, 32.0, 30.9, 9.6, 7.5, 19.6, 16.5, 10.4)
      val sortedList = quickSortAlgorithm.quickSort(unsortedList)
      sortedList should === (controlSortedList)
    }
  }

}
