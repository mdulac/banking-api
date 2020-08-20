package model.commands

import cats.Show
import cats.effect.Sync
import cats.implicits.showInterpolator
import io.circe.generic.auto._
import model.Wallet.WalletId
import model.Wallet.WalletId.decoder
import org.http4s.EntityDecoder
import org.http4s.circe.jsonOf


final case class CreateCardCommand(
                                    walletId: WalletId,
                                  )

object CreateCardCommand {
  implicit def entityDecoder[F[_] : Sync]: EntityDecoder[F, CreateCardCommand] = jsonOf[F, CreateCardCommand]
}

sealed trait CreateCardCommandValidation

object CreateCardCommandValidation {

  import model._

  implicit val show: Show[CreateCardCommandValidation] = {
    case NotWalletOwner(walletId) => show"NotWalletOwner($walletId)"
    case CardCreated(card) => show"CardCreated($card)"
  }

  final case class NotWalletOwner(walletId: WalletId) extends CreateCardCommandValidation

  final case class CardCreated(card: Card) extends CreateCardCommandValidation

  def notWalletOwner(walletId: WalletId): CreateCardCommandValidation = NotWalletOwner(walletId)

  def cardCreated(card: Card): CreateCardCommandValidation = CardCreated(card)

}