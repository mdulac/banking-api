package model.commands

import model.Card.CardId
import model.User.UserId


sealed trait BlockCardCommandValidation

object BlockCardCommandValidation {

  final case class CardUnknown(cardId: String) extends BlockCardCommandValidation

  final case class CardAlreadyBlocked(cardId: CardId) extends BlockCardCommandValidation

  final case class NotCardOwner(userId: UserId, cardId: CardId) extends BlockCardCommandValidation

  final case class CardBlocked(cardId: CardId) extends BlockCardCommandValidation

  def cardUnknown(cardId: String): BlockCardCommandValidation = CardUnknown(cardId)

  def cardAlreadyBlocked(cardId: CardId): BlockCardCommandValidation = CardAlreadyBlocked(cardId)

  def notCardOwner(userId: UserId, cardId: CardId): BlockCardCommandValidation = NotCardOwner(userId: UserId, cardId: CardId)

  def cardBlocked(cardId: CardId): BlockCardCommandValidation = CardBlocked(cardId)
}