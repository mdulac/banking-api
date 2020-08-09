package model

import java.util.UUID

import cats.effect.IO
import doobie.Meta
import io.circe.generic.auto._
import io.circe.{Decoder, Encoder}
import model.Company.CompanyId
import model.Wallet.WalletId
import org.http4s.{EntityEncoder, circe}

final case class Wallet(
                         walletId: WalletId,
                         balance: BigDecimal,
                         currency: Currency,
                         companyId: CompanyId,
                         isMaster: Boolean
                       )

object Wallet {

  implicit val entityEncoder: EntityEncoder[IO, Wallet] = circe.jsonEncoderOf[IO, Wallet]

  final case class WalletId(value: UUID) {
    override def toString: String = value.toString
  }

  object WalletId {
    implicit val walletIdMeta: Meta[WalletId] = Meta[UUID].timap(WalletId.apply)(_.value)
    implicit val encoder: Encoder[WalletId] = Encoder.encodeString.contramap[WalletId](_.toString)
    implicit val decoder: Decoder[WalletId] = Decoder.decodeString.map(x => WalletId(UUID.fromString(x))).withErrorMessage("ERRRROOR")
  }

}