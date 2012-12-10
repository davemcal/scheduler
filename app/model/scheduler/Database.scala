package model

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

object Database {

  val employee = {
    get[Long]("e_id") ~
      get[String]("e_name") map {
        case id ~ name  => new Employee(name, new Time(0), new Time(0), false)
      }
  }

  def all(): List[Employee] = DB.withConnection { implicit c =>
    //SQL("select * from employee").as(employee *)
    List.empty
  }

  def insert(e: Employee) {
    DB.withConnection { implicit c =>
      //SQL("insert into employee (name) values ({name})").on(
      //  'name -> e.name).executeUpdate()
    }
  }

  def delete() {
    DB.withConnection { implicit c =>
      //SQL("delete from employee where 1 = 1").on().executeUpdate()
    }
  }
}