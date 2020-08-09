package model

import java.util.UUID

import doobie.Meta
import model.Company.CompanyId
import model.User.UserId

final case class User(userId: UserId, companyId: CompanyId)

object User {

  final case class UserId(value: UUID) {
    override def toString: String = value.toString
  }

  object UserId {
    implicit val userIdMeta: Meta[UserId] = Meta[UUID].timap(UserId.apply)(_.value)
  }

}
