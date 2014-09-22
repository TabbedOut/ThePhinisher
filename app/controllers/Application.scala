package controllers

import play.api._
import play.api.mvc._
import securesocial.core._
import service.PhabUser

class Application(override implicit val env: RuntimeEnvironment[PhabUser]) extends Controller with SecureSocial[PhabUser] {

  def index = SecuredAction { implicit request =>

    Ok(views.html.index("Your new application is ready."))
  }

}