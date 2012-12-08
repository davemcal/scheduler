package model

object PositionType extends Enumeration {
  type PositionType = Value
  val drivethru = Value("Drive-thru")
  val dropoff = Value("Drop Off")
  val pickup = Value("Pick Up")
  val production = Value("Production")
  val qa = Value("QA")
}

import PositionType._

class Position(val main: PositionType, val number: Int, val description: String, val isPharmacist: Boolean) {
  def ==(that: Position) = main.equals(that.main) && number == that.number
  
  override def toString = main.toString + " " + number + (if (description != "") " (" + description + ")" else "")
}

object Position {
  val allTech = Set(
      PositionType.drivethru,
      PositionType.dropoff,
      PositionType.pickup,
      PositionType.production)
}