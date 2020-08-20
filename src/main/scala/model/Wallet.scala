package model

import java.util.UUID

import cats.Show
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

  final case class WalletId(value: UUID)

  object WalletId {
    implicit val show: Show[WalletId] = (id: WalletId) => id.value.toString

    implicit val walletIdMeta: Meta[WalletId] = Meta[UUID].timap(WalletId.apply)(_.value)
    implicit val encoder: Encoder[WalletId] = Encoder.encodeString.contramap[WalletId](_.toString)
    implicit val decoder: Decoder[WalletId] = Decoder.decodeString.map(x => WalletId(UUID.fromString(x))).withErrorMessage("Error while decoding walletId")

    implicit class WalletIdOps(id: UUID) {
      def walletId: WalletId = WalletId(id)
    }

  }

}