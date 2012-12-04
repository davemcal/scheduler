package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

import model._

object Application extends Controller {
  
  val employeeForm: Form[Seq[Employee]] = Form(
	"employees" -> seq(
	    mapping(
	        "name" -> text,
	        "start" -> text,
	        "end" -> text,
	        "isPharmacist" -> boolean
	    )(Employee.apply)(Employee.unapply)
	)
  )
  
  def index = Action {
    Ok(views.html.index(employeeForm.fill(Schedule.all_employees.sorted.map(new Employee(_, new Time(0), new Time(0), false)))))
  }
  
  def submitForm = Action { implicit request =>
    employeeForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(employeeForm)),
      emps => Ok(views.html.results(new Schedule(emps.filter(e => e.start != new Time(0) || e.end != new Time(0)).toList).displayableSchedule))
      //emps => Ok(views.html.test(emps.filter(e => e.start != new Time(0) || e.end != new Time(0))))
    )
  }
  
}