package model

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

object Database {

  val employee = {
    get[Long]("id") ~
      get[String]("name") ~
      get[Int]("start") ~
      get[Int]("end") ~
      get[Boolean]("ispharmacist") map {
        case id ~ name ~ start ~ end ~ isPharmacist => new Employee(name, new Time(start), new Time(end), isPharmacist)
      }
  }

  def all(): List[Employee] = DB.withConnection { implicit c =>
    SQL("select * from employee").as(employee *)
  }

  def insert(e: Employee) {
    DB.withConnection { implicit c =>
      SQL("insert into employee (name, start, end, ispharmacist) values ({name}, {start}, {end}, {ispharmacist})").on(
        'name -> e.name, 'start -> e.start.minutes, 'end -> e.end.minutes, 'ispharmacist -> e.isPharmacist).executeUpdate()
    }
  }

  def delete() {
    DB.withConnection { implicit c =>
      SQL("delete from employee where 1 = 1").on().executeUpdate()
    }
  }
}