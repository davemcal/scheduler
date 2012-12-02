package model

case class Time(val minutes: Int) extends Ordered[Time] {
  def compare(that: Time) = minutes compare that.minutes
  
  def -(that: Time) = minutes - that.minutes
  
  override def toString = {
    val hour = (minutes / 60) % 12
    val minute = minutes % 60
    val ampm = if (minutes >= (12 * 60)) "pm" else "am"
    
    "%d:%02d".format(if (hour == 0) 12 else hour, minute) + ampm
  }
}

object Time {
  
  def apply(strRep: String) = {
    val timeRegex = """(\d{1,2}):(\d{2})([aApP][mM])""".r

    val timeRegex(h, m, ampm) = strRep

    val offset = if (ampm == "pm") 12 * 60 else 0

    new Time((if (h == "12") 0 else h.toInt) * 60 + m.toInt + offset)
  }
}