package model

import java.time.LocalDateTime
import java.util.UUID

import doobie.Meta
import model.Card.CardId
import model.Transfer.{TransferEntity, TransferId}
import model.Wallet.WalletId

final case class Transfer(
                           id: TransferId,
                           timestamp: LocalDateTime,
                           amount: BigDecimal,
                           originCurrency: Currency,
                           targetCurrency: Currency,
                           conversionFee: Option[BigDecimal],
                           origin: TransferEntity,
                           target: TransferEntity
                         )

object Transfer {

  final case class TransferId(value: UUID) {
    override def toString: String = value.toString
  }

  object TransferId {
    implicit val cardIdMeta: Meta[TransferId] = Meta[UUID].timap(TransferId.apply)(_.value)
  }

  sealed trait TransferEntity

  object TransferEntity {

    final case class CardEntity(cardId: CardId) extends TransferEntity

    final case class WalletEntity(walletId: WalletId) extends TransferEntity

  }

}