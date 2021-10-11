package binary

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DijkstraAlgorithmTest extends AnyWordSpec with Matchers {

  "Dijkstra algorithm" should {
    val dijkstraAlgorithm = new DijkstraAlgorithm()
    "return lowest cost" in {
      val graph: Map[String, Map[String, Int]] = Map("start" -> Map("A" -> 6, "B" -> 2), "A" -> Map("finish" -> 1), "B" -> Map("A" -> 3, "finish" -> 5), "finish" -> Map())
      val result = dijkstraAlgorithm.findLowestWay(graph)
      result should === (6)
    }
    "return lowest cost from harder graph" in {
      val graph: Map[String, Map[String, Int]] = Map(
        "start" -> Map("LP" -> 5, "Poster" -> 0),
        "LP" -> Map("Drums" -> 20, "Bass Guitar" -> 15),
        "Poster" -> Map("Drums" -> 35, "Bass Guitar" -> 30),
        "Bass Guitar" -> Map("finish" -> 20),
        "Drums" -> Map("finish" -> 10),
        "finish" -> Map()
      )
      val result = dijkstraAlgorithm.findLowestWay(graph)
      result should === (35)
    }
  }

}
