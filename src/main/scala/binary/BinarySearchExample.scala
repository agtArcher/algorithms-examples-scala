package binary

class BinarySearchExample {

  def binarySearch[T](list: List[T], item: T): Option[Int] = {
    var low = 0
    var high = list.size - 1
    while (low <= high) {
      val mid = Math.floor((low + high) / 2).toInt
      val guess = list(mid)
      guess.toString.compare(item.toString) match {
        case x if x > 0 =>
          high = mid - 1
        case x if x < 0 =>
          low = mid + 1
        case 0 =>
          return Some(mid)
      }
    }
    low = 0
    high = list.size - 1
    while (low <= high) {
      val mid = Math.floor((low + high) / 2).toInt
      val guess = list(mid)
      guess.toString.compare(item.toString) match {
        case x if x > 0 =>
          low = mid + 1
        case x if x < 0 =>
          high = mid - 1
        case 0 =>
          return Some(mid)
      }
    }
    None
  }
}
