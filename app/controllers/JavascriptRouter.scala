package controllers

import play.api._
import play.api.mvc._

object JavascriptRouter extends Controller {
  def javascriptRoutes = Action { implicit request =>
  Ok(
    Routes.javascriptRouter("jsRoutes")(
      routes.javascript.Burndowns.saveSnapshotViaAjax,
      routes.javascript.Burndowns.updateCompositeMetadataViaAjax
    )
  ).as("text/javascript")
}
}