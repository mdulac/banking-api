package model

import java.time.LocalDate
import java.util.UUID

import cats.Show
import cats.implicits.showInterpolator
import doobie.Meta
import eu.timepit.refined._
import eu.timepit.refined.collection.Size
import eu.timepit.refined.numeric.Interval
import io.circe.Encoder
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

  type Number = Size[Interval.Closed[W.`16`.T, W.`16`.T]]
  type Ccv = Size[Interval.Closed[W.`3`.T, W.`3`.T]]

  implicit val show: Show[Card] = (card: Card) => show"${card.cardId}"

  final case class CardId(value: UUID)

  object CardId {
    implicit val show: Show[CardId] = (id: CardId) => id.value.toString

    implicit val cardIdMeta: Meta[CardId] = Meta[UUID].timap(CardId.apply)(_.value)
    implicit val encoder: Encoder[CardId] = Encoder.encodeString.contramap[CardId](_.toString)

    implicit class CardIdOps(id: UUID) {
      def cardId: CardId = CardId(id)
    }

  }

}