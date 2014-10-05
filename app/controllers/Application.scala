package controllers

import play.api._
import play.api.mvc._
import securesocial.core._
import service.PhabUser

import models.CompositeProject

class Application(override implicit val env: RuntimeEnvironment[PhabUser]) extends Controller with SecureSocial[PhabUser] {

  def index = SecuredAction { implicit request =>

    val projectSnapshots = CompositeProject.listAll
    
    // Only show composites that are started but incomplete
    val inProgress = projectSnapshots.filter(p => p.featureProgress > 0 && p.featureProgress < 100)
    
    
    Ok(views.html.index("Your new application is ready.", inProgress))
  }

}