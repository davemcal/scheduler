package model

object Schedule {
	val all_employees = List(
		"Kim",
		"Lyndsay",
		"Erin",
		"Tricia",
		"Mary Jo",
		"Beth",
		"Vicki",
		"Macy",
		"Laura",
		"Chris",
		"Heather",
		"Kaitlyn",
		"Donald",
		"Megan",
		"Ashlee",
		"Mirette",
		"Lindsay M.",
		"Linda",
		"Sharmilla",
		"Jeff")
	
	val all_pharmacists = List(
	    "Sue",
	    "Chloe",
	    "Karl",
	    "Jen")
}

class Schedule (val employees: List[Employee]) {

  //val employees = employeeStrings.map(new Employee(_))

  def printSchedule = fullSchedule foreach (tuple => {
    val (time, positionList) = tuple

    println(time)

    positionList foreach (tuple => {
      println(tuple._2.name + ": " + tuple._1)
    })

    println
  })

  def printScheduleByEmployee = {
    val sched = fullSchedule

    employees foreach (e => {
      println(e)

      e.historyFilter(sched) foreach (tuple => {
        val (time, pos) = tuple

        println(time + " " + pos)
      })

      println
    })
  }

  def printBoth = {
    val sched = fullSchedule

    sched foreach (tuple => {
      val (time, positionList) = tuple

      println(time)

      positionList foreach (tuple => {
        println(tuple._2.name + ": " + tuple._1)
      })

      println
    })
    
    employees foreach (e => {
      println(e)

      e.historyFilter(sched) foreach (tuple => {
        val (time, pos) = tuple

        println(time + " " + pos)
      })

      println
    })
  }

  def employeesDuring(t: Time) = employees.filter(_.working(t))

  def workerTupleDuring(t: Time) = {
    val tuple = employeesDuring(t).partition(_.isPharmacist)

    (tuple._1.size, tuple._2.size)
  }

  def positionListDuring(t: Time) = {
    val (pharmacists, techs) = workerTupleDuring(t)

    /*if (t >= Time("6:30pm") && t < Time("7:30pm")) {
	  new Position(PositionType.ovorder, 1, "ov order", false) :: positionsMap.getOrElse((pharmacists, techs - 1), List.empty)
	} else*/ positionsMap.getOrElse(workerTupleDuring(t), List.empty)
  }

  def storeOpenTime = employees.map(_.start).min
  def storeCloseTime = employees.map(_.end).max
  def rotateTimes = {
    def rotate0(start: Time): List[Time] =
      if (start < storeCloseTime)
        start :: rotate0(new Time(start.minutes + 2 * 60))
      else
        Nil

    rotate0(storeOpenTime)
  }

  //def employeeChangeTimes = (rotateTimes ++ employees.map(_.start) ++ employees.map(_.end)).sorted.distinct.dropRight(1)

  def employeeChangeDurations = {
    //val times = (Time("6:30pm") :: Time("7:30pm") :: rotateTimes ++ employees.map(_.start) ++ employees.map(_.end)).sorted.distinct
    val times = (rotateTimes ++ employees.map(_.start) ++ employees.map(_.end)).sorted.distinct
    //val times = (storeCloseTime :: rotateTimes).sorted.distinct
    times.sliding(2).toList.map(list => (list.head, list.last - list.head))
  }

  def scoreCurrentSchedule(t: (Time, Int), schedule: List[(Position, Employee)], h: List[(Time, List[(Position, Employee)])]) = {
    schedule.map(tuple => {
      val (pos, emp) = tuple

      emp.score(t, pos, h)
    }).sum
  }
  
  def displayableSchedule = {
    val scheduleMap = fullSchedule.map(tuple => {
      val (time, list) = tuple
      
      (time, list.map(t => (t._2, t._1)).toMap)
    })
    
    (employees.map(_.name).sorted, scheduleMap.map(tuple => {
      val (time, posMap) = tuple
      val posMapString = posMap.map(t => (t._1, t._2.toString))
      
      val (pharms, emps) = posMap.partition(_._1.isPharmacist)
      
      (time, pharms.size, emps.size, employees.sortBy(_.name).map(posMapString.getOrElse(_, "Off")))
    }))
  }

  def fullSchedule = {
    def scheduleFrom(timesLeft: List[(Time, Int)], h: List[(Time, List[(Position, Employee)])]): List[(Time, List[(Position, Employee)])] = {
      if (timesLeft.isEmpty) h
      else {
        val scheduleOptions = fillStream(timesLeft.head, h)

        if (scheduleOptions.isEmpty) {
          println("No schedule at time " + timesLeft.head)

          scheduleFrom(timesLeft.tail, (timesLeft.head._1, List.empty) :: h)
        } else {
          val scheduleForCurrentTime = scheduleOptions.minBy(scoreCurrentSchedule(timesLeft.head, _, h))

          scheduleFrom(timesLeft.tail, (timesLeft.head._1, scheduleForCurrentTime) :: h)
        }
      }
    }

    scheduleFrom(employeeChangeDurations, List.empty).reverse
  }

  def fillStream(t: (Time, Int), h: List[(Time, List[(Position, Employee)])]) = {
    def fill0(p: Position, workerList: Set[Employee]) = {
      val workerStream = workerList.filter(_.isPharmacist == p.isPharmacist).toList.sortBy(_.score(t, p, h)) //.toStream

      //val rotatedStream = workerStream.filter(_.valid(t, p, h))

      workerStream.filter(_.valid(t, p, h)).map((p, _))
    }

    def fillr(
      filled: List[(Position, Employee)],
      positionsLeft: List[Position],
      employeesLeft: Set[Employee]): List[List[(Position, Employee)]] = {

      if (positionsLeft.isEmpty) List(filled)
      else {
        val firstPosStream = fill0(positionsLeft.head, employeesLeft)

        (for {
          (pos, employee) <- firstPosStream
        } yield fillr((pos, employee) :: filled, positionsLeft.tail, employeesLeft - employee)).flatten
      }
    }

    val workers = employeesDuring(t._1).toSet
    val positions = positionListDuring(t._1)

    fillr(List.empty, positions, workers)
  }

  val positionsMap: Map[(Int, Int), List[Position]] = Map(
    (1, 1) -> List(
      new Position(PositionType.pickup, 1, "pick up, drive thru 1, production 1, drop off 2", false),
      new Position(PositionType.qa, 1, "QA - drop off 1, pick up 2, drive thru 2, production 2", true)),
    (1, 2) -> List(
      new Position(PositionType.dropoff, 1, "drop off, pick up 2, drive thru 2", false),
      new Position(PositionType.qa, 1, "QA - drive thru, production 2", true),
      new Position(PositionType.pickup, 1, "pick up 1, production 1", false)),
    (1, 3) -> List(
      new Position(PositionType.dropoff, 1, "drop off", false),
      new Position(PositionType.production, 1, "production 1, drive thru", false),
      new Position(PositionType.qa, 1, "QA - pick up 2, drive thru 2", true),
      new Position(PositionType.pickup, 1, "pick up 1, production 2", false)),
    (1, 4) -> List(
      new Position(PositionType.dropoff, 1, "drop off 1", false),
      new Position(PositionType.production, 1, "production 1, pick up 2", false),
      new Position(PositionType.qa, 1, "QA - drop off 2, drive thru 2", true),
      new Position(PositionType.drivethru, 1, "drive thru", false),
      new Position(PositionType.pickup, 1, "pick up 1, production 2", false)),
    (1, 5) -> List(
      new Position(PositionType.dropoff, 1, "drop off 1", false),
      new Position(PositionType.production, 1, "production 1, drive thru 2", false),
      new Position(PositionType.drivethru, 1, "drive thru", false),
      new Position(PositionType.pickup, 1, "pick up 1", false),
      new Position(PositionType.pickup, 2, "pick up 2, production 2", false),
      new Position(PositionType.qa, 1, "QA - drop off 2, pick up 3", true)),
    (1, 6) -> List(
      new Position(PositionType.dropoff, 1, "drop off 1", false),
      new Position(PositionType.dropoff, 2, "drop off 2, production 3", false),
      new Position(PositionType.production, 1, "production 1, drive thru 2", false),
      new Position(PositionType.drivethru, 1, "drive thru", false),
      new Position(PositionType.pickup, 1, "pick up 1", false),
      new Position(PositionType.pickup, 2, "pick up 2, production 2", false),
      new Position(PositionType.qa, 1, "QA - pick up 3", true)),
    (2, 3) -> List(
      new Position(PositionType.qa, 1, "QA - drop off 1", true),
      new Position(PositionType.production, 1, "production 1, pick up 2", false),
      new Position(PositionType.qa, 2, "QA - drop off 2, drive thru 2", true),
      new Position(PositionType.drivethru, 1, "drive thru", false),
      new Position(PositionType.pickup, 1, "pick up 1, production 2", false)),
    (2, 4) -> List(
      new Position(PositionType.dropoff, 1, "drop off 1", false),
      new Position(PositionType.qa, 2, "QA - drop off 2, production 2", true),
      new Position(PositionType.production, 1, "production 1, pick up 2", false),
      new Position(PositionType.qa, 1, "QA - pick up 3, drive thru 2", true),
      new Position(PositionType.drivethru, 1, "drive thru", false),
      new Position(PositionType.pickup, 1, "pick up 1", false)),
    (2, 5) -> List(
      new Position(PositionType.dropoff, 1, "drop off 1", false),
      new Position(PositionType.qa, 2, "QA - drop off 2", true),
      new Position(PositionType.production, 1, "production 1, drive thru 2", false),
      new Position(PositionType.qa, 1, "QA - pick up 3", true),
      new Position(PositionType.drivethru, 1, "drive thru", false),
      new Position(PositionType.pickup, 1, "pick up 1", false),
      new Position(PositionType.pickup, 2, "pick up 2, production 2", false)),
    (2, 6) -> List(
      new Position(PositionType.dropoff, 1, "drop off 1", false),
      new Position(PositionType.qa, 2, "QA - drop off 2", true),
      new Position(PositionType.production, 1, "production 1", false),
      new Position(PositionType.qa, 1, "QA 1", true),
      new Position(PositionType.drivethru, 1, "drive thru", false),
      new Position(PositionType.pickup, 1, "pick up 1", false),
      new Position(PositionType.pickup, 2, "pick up 2", false),
      new Position(PositionType.pickup, 3, "pick up 3, production 2", false)),
    (2, 7) -> List(
      new Position(PositionType.dropoff, 1, "drop off 1", false),
      new Position(PositionType.dropoff, 2, "drop off 2", false),
      new Position(PositionType.production, 1, "production 1", false),
      new Position(PositionType.qa, 1, "QA 1", true),
      new Position(PositionType.qa, 2, "QA 2", true),
      new Position(PositionType.drivethru, 1, "drive thru", false),
      new Position(PositionType.pickup, 1, "pick up 1", false),
      new Position(PositionType.pickup, 2, "pick up 2", false),
      new Position(PositionType.pickup, 3, "pick up 3, production 2", false)),
    (2, 8) -> List( // temporary
      new Position(PositionType.dropoff, 1, "drop off 1", false),
      new Position(PositionType.dropoff, 2, "drop off 2", false),
      new Position(PositionType.production, 1, "production 1", false),
      new Position(PositionType.production, 2, "production 2", false),
      new Position(PositionType.qa, 1, "QA 1", true),
      new Position(PositionType.qa, 2, "QA 2", true),
      new Position(PositionType.drivethru, 1, "drive thru", false),
      new Position(PositionType.pickup, 1, "pick up 1", false),
      new Position(PositionType.pickup, 2, "pick up 2", false),
      new Position(PositionType.pickup, 3, "pick up 3, production 2", false)),
    (3, 3) -> List(
      new Position(PositionType.dropoff, 1, "drop off 1", false),
      new Position(PositionType.qa, 2, "QA - drop off 2", true),
      new Position(PositionType.production, 1, "production 1, pick up 2", false),
      new Position(PositionType.qa, 1, "QA 1, pick up 3, drive thru 2", true),
      new Position(PositionType.qa, 3, "QA - drive thru", true),
      new Position(PositionType.pickup, 1, "pick up 1", false)),
    (3, 6) -> List(
      new Position(PositionType.dropoff, 1, "drop off 1", false),
      new Position(PositionType.qa, 2, "QA - drop off 2", true),
      new Position(PositionType.production, 1, "production 1", false),
      new Position(PositionType.qa, 1, "QA 1", true),
      new Position(PositionType.qa, 3, "QA 3", true),
      new Position(PositionType.drivethru, 1, "drive thru", false),
      new Position(PositionType.pickup, 1, "pick up 1", false),
      new Position(PositionType.pickup, 2, "pick up 2", false),
      new Position(PositionType.pickup, 3, "pick up 3, production 2", false)),
    (3, 7) -> List(
      new Position(PositionType.dropoff, 1, "drop off 1", false),
      new Position(PositionType.dropoff, 2, "drop off 2", false),
      new Position(PositionType.qa, 3, "QA - drop off 3", true),
      new Position(PositionType.production, 1, "production 1", false),
      new Position(PositionType.qa, 1, "QA 1", true),
      new Position(PositionType.qa, 2, "QA 2", true),
      new Position(PositionType.drivethru, 1, "drive thru", false),
      new Position(PositionType.pickup, 1, "pick up 1", false),
      new Position(PositionType.pickup, 2, "pick up 2", false),
      new Position(PositionType.pickup, 3, "pick up 3, production 2", false)),
    (3, 8) -> List(
      new Position(PositionType.dropoff, 1, "drop off 1", false),
      new Position(PositionType.dropoff, 2, "drop off 2", false),
      new Position(PositionType.dropoff, 3, "drop off 3", false),
      new Position(PositionType.production, 1, "production 1", false),
      new Position(PositionType.production, 2, "production 2, drive thru 2", false),
      new Position(PositionType.qa, 1, "QA 1", true),
      new Position(PositionType.qa, 2, "QA 2", true),
      new Position(PositionType.qa, 3, "QA 3, pickup 3", true),
      new Position(PositionType.drivethru, 1, "drive thru", false),
      new Position(PositionType.pickup, 1, "pick up 1", false),
      new Position(PositionType.pickup, 2, "pick up 2", false)),
    (3, 9) -> List(
      new Position(PositionType.dropoff, 1, "drop off 1", false),
      new Position(PositionType.dropoff, 2, "drop off 2", false),
      new Position(PositionType.production, 1, "production 1", false),
      new Position(PositionType.production, 2, "production 2", false),
      new Position(PositionType.qa, 1, "QA 1", true),
      new Position(PositionType.qa, 2, "QA 2", true),
      new Position(PositionType.qa, 3, "QA 3, data entry, pickup 4", true),
      new Position(PositionType.drivethru, 1, "drive thru 1", false),
      new Position(PositionType.drivethru, 2, "drive thru 2", false),
      new Position(PositionType.pickup, 1, "pick up 1", false),
      new Position(PositionType.pickup, 2, "pick up 2", false),
      new Position(PositionType.pickup, 3, "pick up 3", false)))

}