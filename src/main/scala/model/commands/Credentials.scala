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

sealed trait CredentialsValidation {
  val message: String
}

object CredentialsValidation {

  final case object FieldMissing extends CredentialsValidation {
    override val message: String = "A field is missing"
  }

  final case object UserIdMalformed extends CredentialsValidation {
    override val message: String = "userId is malformed"
  }

  final case object CompanyIdMalformed extends CredentialsValidation {
    override val message: String = "companyId is malformed"
  }

  def validateUserId(value: String): ValidatedNel[CredentialsValidation, UserId] =
    Try(UUID.fromString(value)).map(u => UserId(u).validNel).getOrElse(UserIdMalformed.invalidNel)

  def validateCompanyId(value: String): ValidatedNel[CredentialsValidation, CompanyId] =
    Try(UUID.fromString(value)).map(u => CompanyId(u).validNel).getOrElse(CompanyIdMalformed.invalidNel)
}

