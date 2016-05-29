package org.lyytinen.orbital

import scala.math._

/**
  * A point in 3-dimensional space represented by its cartesian coordinates.
  *
  * @author Jussi Lyytinen
  */
class Point(val x: Double, val y: Double, val z: Double) {

  /**
    * Calculates the euclidean distance to the given point.
    *
    * @param that the given point
    * @return the euclidean distance to the given point
    */
  def distanceTo(that: Point): Double = {
    sqrt(
      pow(this.x - that.x, 2) +
      pow(this.y - that.y, 2) +
      pow(this.z - that.z, 2)
    )
  }
}

object Point {

  /**
    * Constructs a point from the given spherical coordinates.
    *
    * @param lat the latitude in degrees
    * @param lon the longitude in degrees
    * @param alt the altitude in kilometers
    * @return the cartesian representation of the given coordinates
    */
  def fromCoordinates(lat: Double, lon: Double, alt: Double): Point = {

    // The latitude and longitude are given in degrees so here we simply convert
    // them into radians to get the cartesian representation. We also have to
    // remember to add the altitude of the satellite to the radius.

    val r = alt + Constants.EarthsRadius;
    val x = r * cos(lat.toRadians) * cos(lon.toRadians)
    val y = r * cos(lat.toRadians) * sin(lon.toRadians)
    val z = r * sin(lat.toRadians)
    new Point(x, y, z)
  }
}
