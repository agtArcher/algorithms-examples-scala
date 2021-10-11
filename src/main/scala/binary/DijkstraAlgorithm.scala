package binary

import scala.collection.mutable

class DijkstraAlgorithm {

  /**
   * Searches for the lowest cost way.
   *
   * Has to contain 'start' as start point and 'finish' as end point.
   * @param graph graph with values to find lowest cost way
   *
   * @return lowest possible cost.
   */
  def findLowestWay(graph: Map[String, Map[String, Int]]): Int = {
    val parents = mutable.Map[String, String]()
    parents.addAll(graph("start").keys.map(_ -> "start"))
    parents.addOne("finish" -> "No parent")
    val costs = mutable.Map[String, Int]()
    costs.addAll(graph("start")).addOne("finish" -> Double.PositiveInfinity.toInt)

    var processed = Seq[String]()

    var node = findLowestCostNode(costs, processed)
    while (node.isDefined) {
      val cost = costs(node.get)
      val neighbors = graph(node.get)
      neighbors.keys.foreach { n =>
        val newCost = cost + neighbors(n)
        if (costs(n) > newCost) {
          costs(n) = newCost
          parents(n) = node.get
        }
      }
      processed = processed.appended(node.get)
      node = findLowestCostNode(costs, processed)
    }
    costs("finish")
  }

  private def findLowestCostNode(costs: mutable.Map[String, Int], processed: Seq[String]): Option[String] = {
    var lowestCost = Double.PositiveInfinity.toInt
    var lowestCostNode: Option[String] = None
    costs.foreach {
      case (node, cost) =>
        if (cost < lowestCost && !processed.contains(node)) {
          lowestCost = cost
          lowestCostNode = Some(node)
        }
    }
    lowestCostNode
  }

}
