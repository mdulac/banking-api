package model

import java.time.LocalDate
import java.util.UUID

import doobie.Meta
import model.Card.CardId
import model.User.UserId
import model.Wallet.WalletId

final case class Card(
                       cardId: CardId,
                       walletId: WalletId,
                       currency: Currency,
                       balance: BigDecimal,
                       number: String,
                       expirationDate: LocalDate,
                       ccv: String,
                       userId: UserId,
                       isBlocked: Boolean
                     )

object Card {

  final case class CardId(value: UUID) {
    override def toString: String = value.toString
  }

  object CardId {
    implicit val cardIdMeta: Meta[CardId] = Meta[UUID].timap(CardId.apply)(_.value)
  }

}