package service

import play.api.Logger
import securesocial.core._

case class PhabUser(main: BasicProfile, identities: List[BasicProfile]) {

}