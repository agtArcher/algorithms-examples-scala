package binary

class QuickSortExample {

  def quickSort[T](list: List[T])(implicit order: Ordering[T]): List[T] = {
    if (list.size < 2) {
      list
    } else {
      val pivot = list.head
      val tailList = list.tail
      val lessList = filterViaLoop(tailList, (component: T) => order.compare(component, pivot) <= 0)
      val greaterList = filterViaLoop(tailList, (component: T) => order.compare(component, pivot) > 0)
      quickSort(lessList) ++ List(pivot) ++ quickSort(greaterList)
    }
  }

  private def filterViaLoop[T](list: List[T], condition: T => Boolean): List[T] ={
    for {
      element <- list
      if condition(element)
    } yield element
  }

}
