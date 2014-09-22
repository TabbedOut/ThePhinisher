import controllers.CustomRoutesService
import java.lang.reflect.Constructor
import securesocial.core.RuntimeEnvironment
import service.{PhabUser, InMemoryUserService, PhinisherEventListener}
import securesocial.core.providers.GoogleProvider
import scala.collection.immutable.ListMap

object PhinisherGlobal extends play.api.GlobalSettings {

  /**
   * The runtime environment for this app.
   */
  object MyRuntimeEnvironment extends RuntimeEnvironment.Default[PhabUser] {
    override lazy val routes = new CustomRoutesService()
    override lazy val userService: InMemoryUserService = new InMemoryUserService()
    override lazy val eventListeners = List(new PhinisherEventListener())
        override lazy val providers = ListMap(
      // oauth 2 client providers
      include(new GoogleProvider(routes, cacheService,oauth2ClientFor(GoogleProvider.Google)))
    )
    
    
  }

  /**
   * An implementation that checks if the controller expects a RuntimeEnvironment and
   * passes the instance to it if required.
   *
   * This can be replaced by any DI framework to inject it differently.
   *
   * @param controllerClass
   * @tparam A
   * @return
   */
  override def getControllerInstance[A](controllerClass: Class[A]): A = {
    val instance = controllerClass.getConstructors.find { c =>
      val params = c.getParameterTypes
      params.length == 1 && params(0) == classOf[RuntimeEnvironment[PhabUser]]
    }.map {
      _.asInstanceOf[Constructor[A]].newInstance(MyRuntimeEnvironment)
    }
    instance.getOrElse(super.getControllerInstance(controllerClass))
  }
}