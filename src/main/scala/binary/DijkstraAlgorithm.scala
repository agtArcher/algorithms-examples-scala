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
    //set parents for start elements of the graph
    parents.addAll(graph("start").keys.map(_ -> "start"))
    //set 'No parent' for the remaining elements
    graph.foreach {
      case (_, values) => values.keys.map {
        childNode => parents.updateWith(childNode) {
          case Some(v) => Some(v)
          case None => Some("No parent")
        }
      }
    }

    val costs = mutable.Map[String, Int]()
    //set start cost for start elements of the graph.
    costs.addAll(graph("start"))
    //set infinite value for the remaining elements of the graph.
    graph.foreach {
      case (_, values) => values.keys.map {
        childNode => costs.updateWith(childNode) {
          case Some(v) => Some(v)
          case None => Some(Double.PositiveInfinity.toInt)
        }
      }
    }

    var processed = Seq[String]()

    //looping for the lowest costs.
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
