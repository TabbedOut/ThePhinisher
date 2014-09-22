package controllers

import securesocial.controllers.BaseLoginPage
import service.PhabUser
import play.api.Logger
import securesocial.core.{ RuntimeEnvironment, IdentityProvider }
import play.api.mvc.{ RequestHeader, AnyContent, Action }
import securesocial.core.services.RoutesService
import play.api.mvc.Call

class CustomLoginController(implicit override val env: RuntimeEnvironment[PhabUser]) extends BaseLoginPage[PhabUser] {
  lazy val conf = play.api.Play.current.configuration

  override def login: Action[AnyContent] = {
    Logger.debug("using CustomLoginController")
    super.login
  }


}

class CustomRoutesService extends RoutesService.Default {
    override val customCssPath: Option[Call] = {
    val CustomCssKey = "securesocial.customCssPath"
    val path = conf.getString(CustomCssKey).map(securesocial.controllers.routes.Assets.at)
    Logger.info("[securesocial] custom css path = %s".format(path))
    path
  }
  
  override def loginPageUrl(implicit req: RequestHeader): String = controllers.routes.CustomLoginController.login().absoluteURL(IdentityProvider.sslEnabled)
}