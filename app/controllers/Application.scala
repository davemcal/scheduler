package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

import model.Schedule

object Application extends Controller {
  
  val employeeForm = Form(
	"employee" -> list(text)
	)
  
  def index = Action {
    Ok(views.html.index(employeeForm))
  }
  
  def submitForm = Action { implicit request =>
    employeeForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(employeeForm)),
      emps => Ok(views.html.results(new Schedule(emps).fullSchedule))
    )
  }
  
}