package model

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

import java.sql.Date

object Database {

  val employee = {
    get[Long]("eid") ~
      get[String]("ename") ~
      get[Int]("estart") ~
      get[Int]("eend") ~
      get[Boolean]("eispharmacist") map {
        case id ~ name ~ start ~ end ~ isPharmacist  => new Employee(name, new Time(start), new Time(end), isPharmacist)
      }
  }

  def all(day: Date): List[Employee] = DB.withConnection { implicit c =>
    SQL("select * from employee where edate={date}").on('date -> day).as(employee *).sortWith((a, b) => {
      if (b.name == "") true
      else if (a.name == "") false
      else if (b.isPharmacist != a.isPharmacist) a.isPharmacist
      else a.name.toUpperCase < b.name.toUpperCase
    })
  }

  def insert(e: Employee, day: Date) {
    DB.withConnection { implicit c =>
      SQL("insert into employee (ename, estart, eend, eispharmacist, edate) values ({name}, {start}, {end}, {ispharmacist}, {date})").on(
        'name -> e.name, 
        'start -> e.start.minutes, 
        'end -> e.end.minutes, 
        'ispharmacist -> e.isPharmacist, 
        'date -> day).executeUpdate()
      println("inserting " + e)
    }
  }

  def delete(day: Date) {
    DB.withConnection { implicit c =>
      SQL("delete from employee where edate={date}").on('date -> day).executeUpdate()
    }
  }
}