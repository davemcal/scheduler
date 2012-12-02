package model

object PositionType extends Enumeration {
  type PositionType = Value
  val drivethru = Value("drive thru")
  val dropoff = Value("drop off")
  val pickup = Value("pick up")
  val production = Value("production")
  val lunch = Value("lunch break")
  val ovorder = Value("OV order")
  val qa = Value("QA")
}

import PositionType._

class Position(val main: PositionType, val number: Int, val description: String, val isPharmacist: Boolean) {
  def ==(that: Position) = main.equals(that.main) && number == that.number
  
  override def toString = description
}