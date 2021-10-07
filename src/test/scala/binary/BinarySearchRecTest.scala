package binary

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BinarySearchRecTest extends AnyWordSpec with Matchers {

  "binary search algorithm" should {
    val binarySearchAlgorithm = new BinarySearchRecExample()
    "find correct index of string value from string list, sorted ascendant" in {
      val stringList = List("aabc", "aabd", "abbc", "bar", "foo", "lsap")
      val result = binarySearchAlgorithm.binarySearch(stringList, "bar")
      result should === (Some(3))
    }

    "not find index of string value which not exists in string list, sorted ascendant" in {
      val stringList = List("aabc", "aabd", "abbc", "bar", "foo", "lsap")
      val result = binarySearchAlgorithm.binarySearch(stringList, "baz")
      result should === (None)
    }

    "find correct index of double value from list with doubles, sorted descendant" in {
      val doubleList = List(32.1, 32.0, 30.9, 27.6, 24.4, 20.2, 19.6, 16.5, 10.4, 9.6, 7.5)
      val result = binarySearchAlgorithm.binarySearch(doubleList, 24.4)
      result should === (Some(4))
    }

    "not find index of int value which not exists in list with ints, sorted descendant" in {
      val intList = List(14, 12, 10, 7, 6, 4, 3, 1, 0)
      val result = binarySearchAlgorithm.binarySearch(intList, -1)
      result should === (None)
    }
  }
}
