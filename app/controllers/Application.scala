package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

import model._

import java.util.Calendar
import java.sql.Date

object Application extends Controller {

  val employeeForm = Form(tuple(
    "date" -> text,
    "employees" -> seq(
      mapping(
        "name" -> text,
        "start" -> text,
        "end" -> text,
        "isPharmacist" -> boolean)(Employee.apply)(Employee.unapply))))
        
  val employeeFormNoDate = Form(
    "employees" -> seq(
      mapping(
        "name" -> text,
        "start" -> text,
        "end" -> text,
        "isPharmacist" -> boolean)(Employee.apply)(Employee.unapply)))   

  def defaultDate = (Calendar.getInstance().get(Calendar.MONTH) + 1) + "/" +
    Calendar.getInstance().get(Calendar.DAY_OF_MONTH) + "/" +
    Calendar.getInstance().get(Calendar.YEAR)

  def defaultEmployeeList = Schedule.all_pharmacists.sorted.map(new Employee(_, new Time(0), new Time(0), true)) ++
    Schedule.all_employees.sorted.map(new Employee(_, new Time(0), new Time(0), false)) ++
    List("", "").map(new Employee(_, new Time(0), new Time(0), false))

  def index = showForm(
      Calendar.getInstance().get(Calendar.MONTH) + 1,
      Calendar.getInstance().get(Calendar.DAY_OF_MONTH),
      Calendar.getInstance().get(Calendar.YEAR))
  
  def showForm(month: Int, day: Int, year: Int) = Action {
    val emps = Database.all(new Date(year, month, day))
    val formEmps = if (emps.isEmpty || emps.forall(_.name == "")) defaultEmployeeList else emps
    
    Ok(views.html.show(month, day, year, employeeFormNoDate.fill(formEmps)))
  }
  
  def submitForm = submitFormNoDate(
      Calendar.getInstance().get(Calendar.MONTH) + 1,
      Calendar.getInstance().get(Calendar.DAY_OF_MONTH),
      Calendar.getInstance().get(Calendar.YEAR))

  def submitFormNoDate(month: Int, day: Int, year: Int) = Action { implicit request =>
    employeeFormNoDate.bindFromRequest.fold(
      errors => BadRequest(views.html.show(month, day, year, employeeFormNoDate)),
      formResult => {
        val nonEmptyEmployees = formResult.filter(e => e.start != new Time(0) || e.end != new Time(0)).toList
        
        Database.delete(new Date(year, month, day))
        formResult foreach { e => Database.insert(e, new Date(year, month, day)) }
        
        val sched = new Schedule("%d/%d/%d".format(month, day, year), nonEmptyEmployees)

        Ok(views.html.results(sched.dayOfWeek + " " + sched.dateStr, sched.displayableSchedule))
      })
  }
}