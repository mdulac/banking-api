package model

import java.util.UUID

import doobie.Meta
import model.Company.CompanyId

final case class Company(companyId: CompanyId)

object Company {

  final case class CompanyId(value: UUID) {
    override def toString: String = value.toString
  }

  object CompanyId {
    implicit val companyIdMeta: Meta[CompanyId] = Meta[UUID].timap(CompanyId.apply)(_.value)
  }

}