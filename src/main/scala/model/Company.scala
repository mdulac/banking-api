package model

import java.util.UUID

import cats.Show
import doobie.Meta
import io.circe.Encoder
import model.Company.CompanyId

final case class Company(
                          companyId: CompanyId,
                          name: String
                        )

object Company {

  final case class CompanyId(value: UUID)

  object CompanyId {
    implicit val show: Show[CompanyId] = (id: CompanyId) => id.value.toString

    implicit val companyIdMeta: Meta[CompanyId] = Meta[UUID].timap(CompanyId.apply)(_.value)
    implicit val encoder: Encoder[CompanyId] = Encoder.encodeString.contramap[CompanyId](_.toString)

    implicit class CompanyIdOps(id: UUID) {
      def companyId: CompanyId = CompanyId(id)
    }

  }

}