package model.commands

import cats.effect.{IO, Sync}
import io.circe.generic.auto._
import model.Card.CardId
import model.User.UserId
import model.Wallet.WalletId
import org.http4s.EntityDecoder
import org.http4s.circe.jsonOf


final case class LoadCardCommand(
                                  amount: BigDecimal,
                                )

object LoadCardCommand {
  implicit def entityDecoder[F[_] : Sync]: EntityDecoder[F, LoadCardCommand] = jsonOf[F, LoadCardCommand]
}

sealed trait LoadCardCommandValidation

object LoadCardCommandValidation {

  final case class CardUnknown(cardId: String) extends LoadCardCommandValidation

  final case class CardBlocked(cardId: CardId) extends LoadCardCommandValidation

  final case class NotCardOwner(userId: UserId, cardId: CardId) extends LoadCardCommandValidation

  final case class WalletBalanceTooLow(walletId: WalletId, balance: BigDecimal) extends LoadCardCommandValidation

  final case class CardCredited(cardId: CardId, balance: BigDecimal) extends LoadCardCommandValidation

  def cardUnknown(cardId: String): LoadCardCommandValidation = CardUnknown(cardId)

  def cardBlocked(cardId: CardId): LoadCardCommandValidation = CardBlocked(cardId)

  def notCardOwner(userId: UserId, cardId: CardId): LoadCardCommandValidation = NotCardOwner(userId: UserId, cardId: CardId)

  def walletBalanceTooLow(walletId: WalletId, balance: BigDecimal): LoadCardCommandValidation = WalletBalanceTooLow(walletId, balance)

  def cardCredited(cardId: CardId, balance: BigDecimal): LoadCardCommandValidation = CardCredited(cardId, balance)
}