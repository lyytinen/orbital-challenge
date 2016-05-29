package org.lyytinen.orbital

import Constants._
import scala.math._

/**
  * A communication satellite.
  *
  * @author Jussi Lyytinen
  * @param id       the identity of the satellite
  * @param location the location of the satellite
  * @param altitude the altitude of the satellite
  */
class Satellite(val id: String, val location: Point, val altitude: Double) {

  /**
    * The distance to earth's center.
    */
  val radius = Constants.EarthsRadius + altitude;

  /**
    * The angle of the horizon.
    */
  val horizonAngle =
    if (radius == EarthsRadius) {
      0.5 * Pi
    } else
      0.5 * Pi - cos(EarthsRadius / radius)

  /**
    * The distance to the horizon.
    */
  val horizonDistance = sqrt(pow(radius, 2) - pow(EarthsRadius, 2))

  /**
    * Checks if the other satellite is above horizon from our perspective.
    *
    * @param that the other satellite.
    */
  def isAboveHorizon(that: Satellite) = {

    // We can use the law of cosines to calculate the angle between the satellites. There's
    // no need to worry about the exact orientation of them because we are on a plane that
    // is spanned by the altitude vectors of the satellites.

    val angleBetweenSatellites =
      acos((pow(that.location.distanceTo(this.location), 2) + pow(this.radius, 2) - pow(that.radius, 2)) /
        (2 * that.location.distanceTo(this.location) * this.radius))

    angleBetweenSatellites > horizonAngle
  }

  /**
    * Checks if we can transmit to the other satellite.
    *
    * For the transmission to work, the other satellite either has to be above
    * horizon or alternatively it's distance to us has to be less than the distance
    * to the horizon.
    *
    * @param that the other satellite
    */
  def canTransmitTo(that: Satellite) =
    (that.location.distanceTo(this.location) < horizonDistance) || isAboveHorizon(that)

}
