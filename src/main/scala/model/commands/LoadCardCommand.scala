package model.commands

import cats.Show
import cats.effect.Sync
import cats.implicits.showInterpolator
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import io.circe.generic.auto._
import io.circe.refined._
import model.Card.CardId
import model.User.UserId
import model.Wallet.WalletId
import org.http4s.EntityDecoder
import org.http4s.circe.jsonOf
import cats.instances.bigDecimal._


final case class LoadCardCommand(
                                  amount: BigDecimal Refined Positive,
                                )

object LoadCardCommand {
  implicit def entityDecoder[F[_] : Sync]: EntityDecoder[F, LoadCardCommand] = jsonOf[F, LoadCardCommand]
}

sealed trait LoadCardCommandValidation

object LoadCardCommandValidation {

  implicit val show: Show[LoadCardCommandValidation] = {
    case CardUnknown(cardId) => show"CardUnknown($cardId)"
    case CardBlocked(cardId) => show"CardBlocked($cardId)"
    case NotCardOwner(userId, cardId) => show"NotCardOwner($userId, $cardId)"
    case WalletBalanceTooLow(cardId, balance) => show"WalletBalanceTooLow($cardId, $balance)"
    case CardCredited(cardId, balance) => show"CardCredited($cardId, $balance)"
  }

  final case class CardUnknown(cardId: CardId) extends LoadCardCommandValidation

  final case class CardBlocked(cardId: CardId) extends LoadCardCommandValidation

  final case class NotCardOwner(userId: UserId, cardId: CardId) extends LoadCardCommandValidation

  final case class WalletBalanceTooLow(walletId: WalletId, balance: BigDecimal) extends LoadCardCommandValidation

  final case class CardCredited(cardId: CardId, balance: BigDecimal) extends LoadCardCommandValidation

  def cardUnknown(cardId: CardId): LoadCardCommandValidation = CardUnknown(cardId)

  def cardBlocked(cardId: CardId): LoadCardCommandValidation = CardBlocked(cardId)

  def notCardOwner(userId: UserId, cardId: CardId): LoadCardCommandValidation = NotCardOwner(userId: UserId, cardId: CardId)

  def walletBalanceTooLow(walletId: WalletId, balance: BigDecimal): LoadCardCommandValidation = WalletBalanceTooLow(walletId, balance)

  def cardCredited(cardId: CardId, balance: BigDecimal): LoadCardCommandValidation = CardCredited(cardId, balance)
}