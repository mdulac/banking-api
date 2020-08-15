package services

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import cats.Applicative.ops.toAllApplicativeOps
import cats.FlatMap
import cats.effect.Sync
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxOptionId}
import doobie.ConnectionIO
import doobie.implicits._
import doobie.implicits.javatime._
import doobie.util.transactor.Transactor
import fs2.Stream
import model.Card.CardId
import model.Company.CompanyId
import model.Currency.{EUR, GBP, USD}
import model.Transfer.TransferEntity.{CardEntity, WalletEntity}
import model.Transfer.TransferId
import model.User.UserId
import model.Wallet.WalletId
import model._
import model.commands._
import repository.SQLBankingRepository

class BankingService[F[_] : Sync : FlatMap](transactor: Transactor[F], repository: SQLBankingRepository) {

  private val SpendeskFee = 2.9

  private val CurrencyExchange: Map[(Currency, Currency), BigDecimal] = Map(
    (EUR, USD) -> 1.18,
    (EUR, GBP) -> 0.90,
    (USD, EUR) -> 0.85,
    (USD, GBP) -> 0.77,
    (GBP, EUR) -> 1.11,
    (GBP, USD) -> 1.30
  )

  def authenticate(credentials: Credentials): F[AuthenticationStatus] = repository.authenticate(credentials).map {
    case None => AuthenticationStatus.NotAllowed
    case Some((userId, companyId)) => AuthenticationStatus.Authenticated(userId, companyId)
  }.transact(transactor)

  def listCompanies: Stream[F, Company] = repository.listCompanies.transact(transactor)

  def listUsers: Stream[F, User] = repository.listUsers.transact(transactor)

  def listCards(userId: UserId): Stream[F, Card] = repository.listCards(userId).transact(transactor)

  def listWallets(companyId: CompanyId): Stream[F, Wallet] = repository.listWallets(companyId).transact(transactor)

  def createWallet(id: UUID)(companyId: CompanyId)(command: CreateWalletCommand): F[Wallet] =
    repository.createWallet(id)(companyId)(command.balance, command.currency, command.isMaster)
      .map(_ => Wallet(WalletId(id), command.balance, command.currency, companyId, command.isMaster))
      .transact(transactor)

  def createCard(id: UUID, number: String, expirationDate: LocalDate, ccv: String)(userId: UserId, companyId: CompanyId)(command: CreateCardCommand): F[CreateCardCommandValidation] = {
    repository.queryWallet(companyId, command.walletId).flatMap {
      case None => CreateCardCommandValidation.notWalletOwner(command.walletId).pure[ConnectionIO]
      case Some((walletId, _, currency)) => repository.createCard(currency)(id, number, expirationDate, ccv)(userId)(command.walletId) *> CreateCardCommandValidation.cardCreated(Card(CardId(id), walletId, currency, 0, number, expirationDate, ccv, userId, isBlocked = false)).pure[ConnectionIO]
    }.transact(transactor)
  }

  def loadCard(userId: UserId, cardId: String, amount: BigDecimal): F[LoadCardCommandValidation] = {
    repository.queryCard(cardId).flatMap {
      case None => LoadCardCommandValidation.cardUnknown(cardId).pure[ConnectionIO]
      case Some((cardId, ownerId, _, _, _, _)) if ownerId != userId => LoadCardCommandValidation.notCardOwner(userId, cardId).pure[ConnectionIO]
      case Some((cardId, _, _, _, _, true)) => LoadCardCommandValidation.cardBlocked(cardId).pure[ConnectionIO]
      case Some((cardId, _, walletId, cardBalance, currency, false)) =>
        repository.queryWalletBalance(walletId).flatMap {
          case walletBalance if walletBalance < amount => LoadCardCommandValidation.walletBalanceTooLow(walletId, walletBalance).pure[ConnectionIO]
          case walletBalance =>
            val newWalletBalance = walletBalance - amount
            val newCardBalance = cardBalance + amount
            repository.setWalletBalance(walletId)(newWalletBalance) *>
              repository.setCardBalance(cardId)(newCardBalance) *>
              repository.setTransfer(TransferId(UUID.randomUUID()), LocalDateTime.now(), amount, currency, currency, Option.empty, WalletEntity(walletId), CardEntity(cardId)) *>
              LoadCardCommandValidation.cardCredited(cardId, newCardBalance).pure[ConnectionIO]
        }
    }
  }.transact(transactor)

  def blockCard(userId: UserId, cardId: String): F[BlockCardCommandValidation] = {
    repository.queryCard(cardId).flatMap {
      case None => BlockCardCommandValidation.cardUnknown(cardId).pure[ConnectionIO]
      case Some((cardId, ownerId, _, _, _, _)) if ownerId != userId => BlockCardCommandValidation.notCardOwner(userId, cardId).pure[ConnectionIO]
      case Some((cardId, _, _, _, _, true)) => BlockCardCommandValidation.cardAlreadyBlocked(cardId).pure[ConnectionIO]
      case Some((cardId, _, walletId, cardBalance, currency, false)) => repository.blockCard(cardId) *>
        repository.setCardBalance(cardId)(0) *>
        repository.queryWalletBalance(walletId).flatMap(walletBalance => repository.setWalletBalance(walletId)(walletBalance + cardBalance)) *>
        repository.setTransfer(TransferId(UUID.randomUUID()), LocalDateTime.now(), cardBalance, currency, currency, Option.empty, CardEntity(cardId), WalletEntity(walletId)) *>
        BlockCardCommandValidation.cardBlocked(cardId).pure[ConnectionIO]
    }
  }.transact(transactor)

  def unblockCard(userId: UserId, cardId: String): F[UnblockCardCommandValidation] = {
    repository.queryCard(cardId).flatMap {
      case None => UnblockCardCommandValidation.cardUnknown(cardId).pure[ConnectionIO]
      case Some((cardId, ownerId, _, _, _, _)) if ownerId != userId => UnblockCardCommandValidation.notCardOwner(userId, cardId).pure[ConnectionIO]
      case Some((cardId, _, _, _, _, false)) => UnblockCardCommandValidation.cardAlreadyUnblocked(cardId).pure[ConnectionIO]
      case Some((cardId, _, _, _, _, true)) => repository.unblockCard(cardId) *> UnblockCardCommandValidation.cardUnblocked(cardId).pure[ConnectionIO]
    }
  }.transact(transactor)

  def transfer(companyId: CompanyId)(amount: BigDecimal, source: WalletId, target: WalletId): F[TransferCommandValidation] = {
    repository.queryWallet(companyId, source).flatMap {
      case None => TransferCommandValidation.notWalletOwner(source).pure[ConnectionIO]
      case Some((sourceId, sourceBalance, _)) if sourceBalance < amount => TransferCommandValidation.walletBalanceTooLow(sourceId, sourceBalance).pure[ConnectionIO]
      case Some((sourceId, sourceBalance, sourceCurrency)) =>
        repository.queryWallet(companyId, target).flatMap {
          case None => TransferCommandValidation.walletUnknown(target).pure[ConnectionIO]
          case Some((targetId, targetBalance, targetCurrency)) if sourceCurrency == targetCurrency =>
            val transferId = TransferId(UUID.randomUUID())
            val timestamp = LocalDateTime.now()
            repository.setWalletBalance(sourceId)(sourceBalance - amount) *>
              repository.setWalletBalance(targetId)(targetBalance + amount) *>
              repository.setTransfer(transferId, timestamp, amount, sourceCurrency, targetCurrency, Option.empty, WalletEntity(sourceId), WalletEntity(targetId)) *>
              TransferCommandValidation.transfered(Transfer(transferId, timestamp, amount, sourceCurrency, targetCurrency, Option.empty, WalletEntity(sourceId), WalletEntity(targetId))).pure[ConnectionIO]
          case Some((targetId, targetBalance, targetCurrency)) =>
            val transferId = TransferId(UUID.randomUUID())
            val timestamp = LocalDateTime.now()
            val exchange = CurrencyExchange((sourceCurrency, targetCurrency))
            val amountWithExchange = amount * exchange
            val spendeskFee = (SpendeskFee / 100) * amountWithExchange
            val amountToCredit = amountWithExchange - spendeskFee
            repository.setWalletBalance(sourceId)(sourceBalance - amount) *>
              repository.setWalletBalance(targetId)(targetBalance + amountToCredit) *>
              repository.setTransfer(transferId, timestamp, amount, sourceCurrency, targetCurrency, spendeskFee.some, WalletEntity(sourceId), WalletEntity(targetId)) *>
              repository.queryMasterWallet(targetCurrency).flatMap {
                case (masterWalletId, masterWalletBalance) =>
                  repository.setWalletBalance(masterWalletId)(masterWalletBalance + spendeskFee) *>
                    repository.setTransfer(TransferId(UUID.randomUUID()), timestamp, spendeskFee, targetCurrency, targetCurrency, Option.empty, WalletEntity(targetId), WalletEntity(masterWalletId))
              } *>
              TransferCommandValidation.transfered(Transfer(transferId, timestamp, amount, sourceCurrency, targetCurrency, spendeskFee.some, WalletEntity(sourceId), WalletEntity(targetId))).pure[ConnectionIO]
        }
    }
  }.transact(transactor)

}
