package model

object TimeOfDay {
  val AM = 0
  val PM = 12 * 60
}

import TimeOfDay._

case class Time(val minutes: Int) extends Ordered[Time] {
  def compare(that: Time) = minutes compare that.minutes

  def -(that: Time) = minutes - that.minutes

  def advance(length: Int) = new Time(minutes + length)
  
  override def toString = {
    if (minutes == 0) "" else {
      val hour = (minutes / 60) % 12
      val minute = minutes % 60
      val ampm = if (minutes >= (12 * 60)) "pm" else "am"

      "%d:%02d".format(if (hour == 0) 12 else hour, minute) + ampm
    }
  }
}

object Time {

  def apply(strRep: String, defaultTimeOfDay: Int = TimeOfDay.AM) = {
    def adjustedOffset(hour: Int) = {
      if (hour <= 7 || hour == 12) TimeOfDay.PM
      else if (hour == 11) TimeOfDay.AM
      else defaultTimeOfDay
    }

    val timeRegex = """(\d{1,2}):(\d{2})([aApP][mM])?""".r
    val simpleTimeRegex = """(\d{1,2})([aApP][mM])?""".r

    val (hour, minute, ampmOffset) = strRep match {
      case timeRegex(h, m, null) => (h.toInt, m.toInt, adjustedOffset(h.toInt))
      case timeRegex(h, m, ampm) => (h.toInt, m.toInt, if (ampm.toLowerCase() == "pm") 12 * 60 else 0)
      case simpleTimeRegex(h, null) => (h.toInt, 0, adjustedOffset(h.toInt))
      case simpleTimeRegex(h, ampm) => (h.toInt, 0, if (ampm.toLowerCase() == "pm") 12 * 60 else 0)
      case _ => (0, 0, 0)
    }

    new Time((if (hour == 12) 0 else hour) * 60 + minute + ampmOffset)
  }
}