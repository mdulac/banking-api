package model

import java.util.UUID

import doobie.Meta
import io.circe.Encoder
import model.Company.CompanyId
import model.User.UserId
import model.Wallet.WalletId

final case class User(userId: UserId, companyId: CompanyId)

object User {

  final case class UserId(value: UUID) {
    override def toString: String = value.toString
  }

  object UserId {
    implicit val userIdMeta: Meta[UserId] = Meta[UUID].timap(UserId.apply)(_.value)
    implicit val encoder: Encoder[UserId] = Encoder.encodeString.contramap[UserId](_.toString)

    implicit class UserIdOps(id: UUID) {
      def userId: UserId = UserId(id)
    }

  }

}
