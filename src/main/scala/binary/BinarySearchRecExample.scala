package binary

import scala.annotation.tailrec

class BinarySearchRecExample {

  def binarySearch[T](list: List[T], item: T): Option[Int] = {
    list.head.toString.compare(list.last.toString) match {
      case x if x < 0 =>
        binarySearchAscRec(list, item, 0)
      case x if x > 0 =>
        binarySearchDescRec(list, item, 0)
      case 0 =>
        if (list.head == item) Some(0) else None
    }
  }

  @tailrec
  private def binarySearchAscRec[T](list: List[T], item: T, startIndex: Int): Option[Int] = {
    if (list.size == 1 && list.head != item) {
      None
    }
    else {
      val mid = Math.floor((list.size - 1) / 2).toInt
      list(mid).toString.compare(item.toString) match {
        case x if x > 0 => binarySearchAscRec(list.slice(0, mid), item, startIndex)
        case x if x < 0 => binarySearchAscRec(list.slice(mid, list.size), item, startIndex + mid)
        case 0 => Some(startIndex + mid)
      }
    }
  }

  @tailrec
  private def binarySearchDescRec[T](list: List[T], item: T, startIndex: Int): Option[Int] = {
    if (list.size == 1 && list.head != item) {
      None
    }
    else {
      val mid = Math.floor((list.size - 1) / 2).toInt
      list(mid).toString.compare(item.toString) match {
        case x if x < 0 => binarySearchDescRec(list.slice(0, mid), item, startIndex)
        case x if x > 0 => binarySearchDescRec(list.slice(mid, list.size), item, startIndex + mid)
        case 0 => Some(startIndex + mid)
      }
    }
  }

}
