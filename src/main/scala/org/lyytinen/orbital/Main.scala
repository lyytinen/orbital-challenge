package org.lyytinen.orbital

/**
  * My solution for the Reaktor Orbital Challenge.
  *
  * @author Jussi Lyytinen
  */
object Main {

  /**
    * Reads a list of satellites from the data file.
    *
    * @param fileName the filename
    * @return a list of satellites parsed from the input file
    */
  def readSatellites(fileName: String): List[Satellite] = {
    val dataSource = io.Source.fromURL(getClass.getResource(fileName))
    val lines = dataSource.getLines.toList.drop(1).dropRight(1)
    lines.map( line => {
      val cols = line.split(",").map(_.trim)
      val location = Point.fromCoordinates(cols(1).toDouble, cols(2).toDouble, cols(3).toDouble)
      new Satellite(cols(0), location, cols(3).toDouble)
    })
  }

  /**
    * Reads both the start and endpoint from the data file.
    *
    * To avoid handling them as special cases, they are also modeled as satellites.
    *
    * @param fileName the filename
    */
  def readEndpoints(fileName: String): (Satellite, Satellite) = {
    val dataSource = io.Source.fromURL(getClass.getResource(fileName))
    val line = dataSource.getLines.toList.last
    val cols = line.split(",").map(_.trim)

    val locations = (
      Point.fromCoordinates(cols(1).toDouble, cols(2).toDouble, 0),
      Point.fromCoordinates(cols(3).toDouble, cols(4).toDouble, 0))

    (new Satellite("start", locations._1, 0), new Satellite("end", locations._2, 0))
  }

  /**
    * Finds the shortest chain of satellites that can relay a transmission between
    * two points on the surface of the earth.
    *
    * We limit the depth of the search by keeping track of the current optima. If
    * the depth of the current search branch grows larger than the current optima
    * we can stop the search early in that particular branch.
    *
    * @param route the current route (can be partial)
    * @param optima the current optima; or Nil if no route has been found yet
    * @param unvisited a list of satellites not searched yet
    * @param end the endpoint of our transmission
    * @return an optimal route to the endpoint
    */
  def findRoute(route: List[Satellite],
                optima: List[Satellite],
                unvisited: List[Satellite],
                end: Satellite) : List[Satellite] = {

    if (route.size == optima.size || unvisited.isEmpty) {
      optima
    } else if (route.last.canTransmitTo(end)) {
      route ++ List(end)
    } else {
      val split = unvisited.partition(_.canTransmitTo(route.last))
      split._1 match {
        case Nil => optima
        case head :: tail => {

          // Here we can limit the breadth of the search by simply selecting the first
          // satellite we can transmit to and continuing the search only to those we cannot
          // transmit from here. This makes sense because it is not possible to find
          // an optima by hopping between satellites that are all visible to each other.

          val firstBranchOptima = findRoute (route ++ List(head), optima, split._2, end)

          // Using the same logic as above, we can now continue to search the other
          // transmittable satellites but this time we remove the first satellite from
          // the unvisited ones. If the optimal route goes through that satellite, we
          // have already found the optima at this point.

          findRoute(route, firstBranchOptima, unvisited.diff(List(head)), end)
        }
      }
    }
  }

  /**
    * The main entry-point.
    */
  def main(args: Array[String]): Unit = {

    val fileName = "/satellite-data-1.csv"
    val satellites = readSatellites(fileName)
    val endpoints = readEndpoints(fileName)
    val route = findRoute(List(endpoints._1), Nil, satellites, endpoints._2)
    println(route.drop(1).dropRight(1).map(_.id).mkString(","))
  }
}
