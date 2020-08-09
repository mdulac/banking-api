package model.commands

import model.Card.CardId
import model.User.UserId


sealed trait UnblockCardCommandValidation

object UnblockCardCommandValidation {

  final case class CardUnknown(cardId: String) extends UnblockCardCommandValidation

  final case class CardAlreadyUnblocked(cardId: CardId) extends UnblockCardCommandValidation

  final case class NotCardOwner(userId: UserId, cardId: CardId) extends UnblockCardCommandValidation

  final case class CardUnblocked(cardId: CardId) extends UnblockCardCommandValidation

  def cardUnknown(cardId: String): UnblockCardCommandValidation = CardUnknown(cardId)

  def cardAlreadyUnblocked(cardId: CardId): UnblockCardCommandValidation = CardAlreadyUnblocked(cardId)

  def notCardOwner(userId: UserId, cardId: CardId): UnblockCardCommandValidation = NotCardOwner(userId: UserId, cardId: CardId)

  def cardUnblocked(cardId: CardId): UnblockCardCommandValidation = CardUnblocked(cardId)
}