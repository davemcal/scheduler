package model

object Employee {
  def apply(name: String, start: String, end: String, isPharmacist: Boolean = false) = {
	  new Employee(name, Time(start, TimeOfDay.AM), Time(end, TimeOfDay.PM), isPharmacist)
  }

  def apply(strRep: String) = {
    val empRegex = """([a-zA-Z0-9]+)\s*(\d{1,2}:\d{2}[apAP][mM])\s*-\s*(\d{1,2}:\d{2}[apAP][mM])\s*(P)?""".r

    val (name, start, end, isPharmacist) = {
      val empRegex(name_str, start_str, end_str, pharmacist) = strRep

      (name_str, Time(start_str), Time(end_str), pharmacist != null)
    }
    
    new Employee(name, start, end, isPharmacist)
  }
  
  def unapply(e: Employee) : Option[(String, String, String, Boolean)] = {
    Some((e.name, e.start.toString, e.end.toString, e.isPharmacist))
  }
  
  val workRestrictions = Map(
    "Ashlee" -> Set(
      (PositionType.pickup, 1),
      (PositionType.pickup, 2),
      (PositionType.pickup, 3),
      (PositionType.drivethru, 1),
      (PositionType.drivethru, 2),
      (PositionType.production, 1),
      (PositionType.production, 2),
      (PositionType.production, 3)),
    "Megan" -> Set(
      (PositionType.pickup, 1),
      (PositionType.pickup, 2),
      (PositionType.pickup, 3),
      (PositionType.drivethru, 1),
      (PositionType.drivethru, 2),
      (PositionType.production, 1),
      (PositionType.production, 2),
      (PositionType.production, 3)),
    "Kaitlyn" -> Set(
      (PositionType.drivethru, 1),
      (PositionType.drivethru, 2),
      (PositionType.dropoff, 2),
      (PositionType.pickup, 1),
      (PositionType.pickup, 2),
      (PositionType.pickup, 3),
      (PositionType.production, 1),
      (PositionType.production, 2),
      (PositionType.production, 3)),
    "Macy" -> Set(
      (PositionType.drivethru, 1),
      (PositionType.drivethru, 2),
      (PositionType.dropoff, 2),
      (PositionType.pickup, 1),
      (PositionType.pickup, 2),
      (PositionType.pickup, 3),
      (PositionType.production, 1),
      (PositionType.production, 2),
      (PositionType.production, 3)))
}

class Employee(val name: String, val start: Time, val end: Time, val isPharmacist: Boolean) {

  override def toString = "%s (%s - %s) %s".format(name, start, end, if (isPharmacist) "[Pharmacist]" else "")

  def working(t: Time) = start <= t && end > t

  val validPositions = Employee.workRestrictions.getOrElse(name, Position.allTech)
  
  def valid(t: (Time, Int), p: Position, h: List[(Time, List[(Position, Employee)])]) = {
    if (isPharmacist) true else validPositions.contains((p.main, p.number)) && score(t, p, h) < 51
  }

  def score(t: (Time, Int), p: Position, h: List[(Time, List[(Position, Employee)])]) = {
    val filteredHistory = historyFilter(h)

    if (isPharmacist) {
      if (isSamePosition(p, filteredHistory)) 0 else 5
    } else {
      if (isSamePosition(p, filteredHistory)) {
        val timeWorked = (t._1 - currentPosition(filteredHistory).get._1) + t._2
        if (timeWorked <= 60) -30
        else if (timeWorked < 120) timeWorked / 10 - 12
        else if (timeWorked < 150) 3
        else (timeWorked / 40) * (timeWorked / 40)
      } else if (isEquivalentPosition(p, filteredHistory)) 100
      else if (timeUntilStopWorking(t._1) < 60) 50
      else if (isRepeat(p, filteredHistory)) alreadyWorkedPenalty(t._1, p, filteredHistory)
      else currentPosition(filteredHistory) match {
        case None => 0
        case Some((cur_time, cur_pos)) => {
          if (t._1 - cur_time < 60) 50
          else {
            if (cur_pos.main == PositionType.drivethru) {
              if (p.main == PositionType.pickup) -5 else 0
            } else if (cur_pos.main == PositionType.pickup) {
              if (p.main == PositionType.drivethru) -5 else 0
            } else 0
          }
        }
      }
    }
  }

  def timeUntilStopWorking(t: Time) = end - t

  def isRepeat(p: Position, h: List[(Time, Position)]) = {
    h.exists(_._2.main == p.main)
  }

  def alreadyWorkedPenalty(currentTime: Time, p: Position, h: List[(Time, Position)]) = {
    val latestTime = h.filter(_._2.main == p.main).map(_._1).max

    if (currentTime - latestTime >= 6 * 60) 0
    else (currentTime - latestTime) / 10
  }

  def isSamePosition(p: Position, h: List[(Time, Position)]) = {
    currentPosition(h) match {
      case None => false
      case Some((time, pos)) => p.main == pos.main && p.number == pos.number
    }
  }

  def isEquivalentPosition(p: Position, h: List[(Time, Position)]) = {
    currentPosition(h) match {
      case None => false
      case Some((time, pos)) => p.main == pos.main
    }
  }

  def currentPosition(h: List[(Time, Position)]) = {
    if (h.isEmpty) None
    else Some(h.maxBy(_._1))
  }

  def historyFilter(h: List[(Time, List[(Position, Employee)])]): List[(Time, Position)] = {
    val baseTimes = h.collect(tuple => tuple._2.find(_._2 == this) match {
      case Some((pos, _)) => (tuple._1, pos)
    }).reverse

    def addTime(list: List[(Time, Position)], next: (Time, Position)): List[(Time, Position)] = {
      if (!list.isEmpty && list.head._2 == next._2) {
        if (list.head._1 < next._1) list
        else next :: list.tail
      } else next :: list
    }

    baseTimes.foldLeft(List.empty: List[(Time, Position)])(addTime)
  }
}