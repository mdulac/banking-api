package model

import model.Company.CompanyId
import model.User.UserId

sealed trait AuthenticationStatus extends Product with Serializable

object AuthenticationStatus {

  final case class Authenticated(userId: UserId, companyId: CompanyId) extends AuthenticationStatus

  final case class NotAllowed(userId: UserId, companyId: CompanyId) extends AuthenticationStatus

}