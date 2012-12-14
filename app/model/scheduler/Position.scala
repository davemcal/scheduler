package model

object PositionType extends Enumeration {
  type PositionType = Value
  val drivethru = Value("Drive-thru")
  val dropoff = Value("Drop Off")
  val pickup = Value("Pick Up")
  val production = Value("Production")
  val qa = Value("QA")
  val off = Value("Off")
}

import PositionType._

class Position(val main: PositionType, val number: Int, val description: String, val isPharmacist: Boolean) {
  def ==(that: Position) = main.equals(that.main) && number == that.number
  
  override def toString = {
    val mainStr = main.toString + (if (number > 0) " " + number else "")
    
    mainStr + (if (description != "") " (" + description + ")" else "")
  }
}

object Position {
  val allTech = Set(
      (PositionType.drivethru, 1),
      (PositionType.drivethru, 2),
      (PositionType.dropoff, 1),
      (PositionType.dropoff, 2),
      (PositionType.pickup, 1),
      (PositionType.pickup, 2),
      (PositionType.pickup, 3),
      (PositionType.production, 1),
      (PositionType.production, 2),
      (PositionType.production, 3))
}