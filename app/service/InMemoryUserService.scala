package service

import play.api.Logger
import securesocial.core._
import securesocial.core.providers.{ UsernamePasswordProvider, MailToken }
import scala.concurrent.Future
import securesocial.core.services.{ UserService, SaveMode }
import scala.concurrent.ExecutionContext.Implicits.global

import anorm._
import play.api.db.DB

/**
 * A Sample In Memory user service in Scala
 *
 * IMPORTANT: This is just a sample and not suitable for a production environment since
 * it stores everything in memory.
 */
class InMemoryUserService extends UserService[PhabUser] {
  val logger = Logger("application.controllers.InMemoryUserService")

  //
  var users = Map[(String, String), PhabUser]()
  //private var identities = Map[String, BasicProfile]()
  private var tokens = Map[String, MailToken]()

  def find(providerId: String, userId: String): Future[Option[BasicProfile]] = {
    if (logger.isDebugEnabled || true) {
      logger.debug("users = %s".format(users))
    }
    val result = for (
      user <- users.values;
      basicProfile <- user.identities.find(su => su.providerId == providerId && su.userId == userId)
    ) yield {
      basicProfile
    }
    Future.successful(result.headOption)
  }

  def findByEmailAndProvider(email: String, providerId: String): Future[Option[BasicProfile]] = {
    if (logger.isDebugEnabled || true) {
      logger.debug("users = %s".format(users))
    }
    val someEmail = Some(email)
    val result = for (
      user <- users.values;
      basicProfile <- user.identities.find(su => su.providerId == providerId && su.email == someEmail)
    ) yield {
      basicProfile
    }
    Future.successful(result.headOption)
  }

  def userAllowedToLogIn(user: BasicProfile): Boolean = {
    import play.api.Play.current
    
    // TODO: This has only been confirmed with google SSO, other providers
    // may not work as seamlessly
    
    DB.withConnection("phabricator") { implicit c =>

      val sqlQuery = SQL(
        """
          select userPHID from phabricator_user.user_externalaccount extacc
          left join phabricator_user.user u on extacc.userPHID=u.phid
          where accountType={provider}
          and isApproved=1
          and isDisabled=0
          AND accountID = {account_id}
          ;
        """).on('account_id -> user.email, 'provider -> user.providerId)

      val matchedUsers = sqlQuery().map(row =>
        row[String]("userPHID")).toList

      matchedUsers.nonEmpty
    }
 
  }

  def save(user: BasicProfile, mode: SaveMode): Future[PhabUser] = {

    if (userAllowedToLogIn(user)) {
      mode match {
        case SaveMode.SignUp =>
          val newUser = PhabUser(user, List(user))
          users = users + ((user.providerId, user.userId) -> newUser)
        case SaveMode.LoggedIn =>

      }
      // first see if there is a user with this BasicProfile already.
      val maybeUser = users.find {
        case (key, value) if value.identities.exists(su => su.providerId == user.providerId && su.userId == user.userId) => true
        case _ => false
      }
      maybeUser match {
        case Some(existingUser) =>
          val identities = existingUser._2.identities
          val updatedList = identities.patch(identities.indexWhere(i => i.providerId == user.providerId && i.userId == user.userId), Seq(user), 1)
          val updatedUser = existingUser._2.copy(identities = updatedList)
          users = users + (existingUser._1 -> updatedUser)
          Future.successful(updatedUser)

        case None =>
          val newUser = PhabUser(user, List(user))
          users = users + ((user.providerId, user.userId) -> newUser)
          Future.successful(newUser)
      }
    } else {
      // "User does not exist in phabricator"
      Future.failed(AuthenticationException())
    }
  }

  def link(current: PhabUser, to: BasicProfile): Future[PhabUser] = {
    if (current.identities.exists(i => i.providerId == to.providerId && i.userId == to.userId)) {
      Future.successful(current)
    } else {
      val added = to :: current.identities
      val updatedUser = current.copy(identities = added)
      users = users + ((current.main.providerId, current.main.userId) -> updatedUser)
      Future.successful(updatedUser)
    }
  }

  def saveToken(token: MailToken): Future[MailToken] = {
    Future.successful {
      tokens += (token.uuid -> token)
      token
    }
  }

  def findToken(token: String): Future[Option[MailToken]] = {
    Future.successful { tokens.get(token) }
  }

  def deleteToken(uuid: String): Future[Option[MailToken]] = {
    Future.successful {
      tokens.get(uuid) match {
        case Some(token) =>
          tokens -= uuid
          Some(token)
        case None => None
      }
    }
  }

  //  def deleteTokens(): Future {
  //    tokens = Map()
  //  }

  def deleteExpiredTokens() {
    tokens = tokens.filter(!_._2.isExpired)
  }

  override def updatePasswordInfo(user: PhabUser, info: PasswordInfo): Future[Option[BasicProfile]] = {
    Future.successful {
      for (
        found <- users.values.find(_ == user);
        identityWithPasswordInfo <- found.identities.find(_.providerId == UsernamePasswordProvider.UsernamePassword)
      ) yield {
        val idx = found.identities.indexOf(identityWithPasswordInfo)
        val updated = identityWithPasswordInfo.copy(passwordInfo = Some(info))
        val updatedIdentities = found.identities.patch(idx, Seq(updated), 1)
        found.copy(identities = updatedIdentities)
        updated
      }
    }
  }

  override def passwordInfoFor(user: PhabUser): Future[Option[PasswordInfo]] = {
    Future.successful {
      for (
        found <- users.values.find(_ == user);
        identityWithPasswordInfo <- found.identities.find(_.providerId == UsernamePasswordProvider.UsernamePassword)
      ) yield {
        identityWithPasswordInfo.passwordInfo.get
      }
    }
  }
}