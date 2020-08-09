package model.commands

import java.util.UUID

import cats.data.ValidatedNel
import cats.implicits.catsSyntaxValidatedId
import io.circe.Codec
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveEnumerationCodec
import model.Company.CompanyId
import model.User.UserId

import scala.util.Try

final case class Credentials(
                              userId: UserId,
                              companyId: CompanyId
                            )

object Credentials {
  private implicit val config: Configuration =
    Configuration.default.copy(transformConstructorNames = _.toLowerCase)

  implicit val validationCodec: Codec[CredentialsValidation] = deriveEnumerationCodec[CredentialsValidation]
}

sealed trait CredentialsValidation

object CredentialsValidation {

  final case object FieldMissing extends CredentialsValidation

  final case object UserIdMalformed extends CredentialsValidation

  final case object CompanyIdMalformed extends CredentialsValidation

  def validateUserId(value: String): ValidatedNel[CredentialsValidation, UserId] =
    Try(UUID.fromString(value)).map(u => UserId(u).validNel).getOrElse(UserIdMalformed.invalidNel)

  def validateCompanyId(value: String): ValidatedNel[CredentialsValidation, CompanyId] =
    Try(UUID.fromString(value)).map(u => CompanyId(u).validNel).getOrElse(CompanyIdMalformed.invalidNel)
}

