package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

import model._

import java.util.Calendar

object Application extends Controller {

  val employeeForm = Form(tuple(
    "date" -> text,
    "employees" -> seq(
      mapping(
        "name" -> text,
        "start" -> text,
        "end" -> text,
        "isPharmacist" -> boolean)(Employee.apply)(Employee.unapply))))

  def index = Action {
    Ok(views.html.index(employeeForm.fill(((Calendar.getInstance().get(Calendar.MONTH) + 1) + "/" + Calendar.getInstance().get(Calendar.DAY_OF_MONTH) + "/" + Calendar.getInstance().get(Calendar.YEAR),
      Schedule.all_pharmacists.sorted.map(new Employee(_, new Time(0), new Time(0), true)) ++
      Schedule.all_employees.sorted.map(new Employee(_, new Time(0), new Time(0), false)) ++
      List("", "").map(new Employee(_, new Time(0), new Time(0), false))))))
  }

  def submitForm = Action { implicit request =>
    employeeForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(employeeForm)),
      formResult => {
        val sched = new Schedule(formResult._1, formResult._2.filter(e => e.start != new Time(0) || e.end != new Time(0)).toList)

        Ok(views.html.results(sched.dayOfWeek + " " + sched.dateStr, sched.displayableSchedule)) 
      })
  }

}