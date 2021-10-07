package binary

class QuickSortExample {

  def quickSort[T](list: List[T])(implicit order: Ordering[T]): List[T] = {
    if (list.size < 2) {
      list
    } else {
      val pivot = list.head
      val tailList = list.tail
      val lessList = tailList.filter(component => order.compare(component, pivot) <= 0)
      val greaterList = tailList.filter(component => order.compare(component, pivot) > 0)
      quickSort(lessList) ++ List(pivot) ++ quickSort(greaterList)
    }
  }

}
