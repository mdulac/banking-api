package services

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import cats.Applicative.ops.toAllApplicativeOps
import cats.FlatMap
import cats.effect.Sync
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxOptionId}
import cats.syntax.flatMap.toFlatMapOps
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import io.chrisdavenport.log4cats.Logger
import model.Card.CardId
import model.Company.CompanyId
import model.Currency.{EUR, GBP, USD}
import model.Transfer.TransferEntity.{CardEntity, WalletEntity}
import model.Transfer.TransferId
import model.User.UserId
import model.Wallet.WalletId
import model._
import model.commands._
import repository.BankingRepository

class BankingService[F[_] : Sync : FlatMap : Logger, Q[_]](repository: BankingRepository[Q, F]) {

  import repository.{Instance, Transform, transact}

  private val Fee = 2.9

  private val CurrencyExchange: Map[(Currency, Currency), BigDecimal] = Map(
    (EUR, USD) -> 1.18,
    (EUR, GBP) -> 0.90,
    (USD, EUR) -> 0.85,
    (USD, GBP) -> 0.77,
    (GBP, EUR) -> 1.11,
    (GBP, USD) -> 1.30
  )

  def authenticate(credentials: Credentials): F[AuthenticationStatus] = transact {
    repository.authenticate(credentials).map {
      case None => AuthenticationStatus.NotAllowed(credentials.userId, credentials.companyId)
      case Some((userId, companyId)) => AuthenticationStatus.Authenticated(userId, companyId)
    }
  }

  def listCompanies: F[List[Company]] =
    Logger[F].info(s"List companies") *> transact {
      repository.listCompanies
    }

  def listUsers: F[List[User]] =
    Logger[F].info(s"List users") *> transact {
      repository.listUsers
    }

  def listCards(userId: UserId): F[List[Card]] =
    Logger[F].info(s"List cards") *> transact {
      repository.listCards(userId)
    }

  def listWallets(companyId: CompanyId): F[List[Wallet]] =
    Logger[F].info(s"List wallets") *> transact {
      repository.listWallets(companyId)
    }

  def createWallet(id: UUID)(companyId: CompanyId)(command: CreateWalletCommand): F[Wallet] =
    Logger[F].info(s"Create wallet $id") *> transact {
      repository.createWallet(id)(companyId)(command.balance.value, command.currency, command.isMaster)
        .map(_ => Wallet(WalletId(id), command.balance.value, command.currency, companyId, command.isMaster))
    }

  def createCard(cardId: CardId, number: String, expirationDate: LocalDate, ccv: String, userId: UserId, companyId: CompanyId)(command: CreateCardCommand): F[CreateCardCommandValidation] =
    Logger[F].info(s"Create card $cardId") *> transact {
      repository.queryWallet(companyId, command.walletId).flatMap {
        case None => CreateCardCommandValidation.notWalletOwner(command.walletId).pure[Q]
        case Some((walletId, _, currency)) => repository.createCard(currency)(cardId, number, expirationDate, ccv)(userId)(command.walletId) *> CreateCardCommandValidation.cardCreated(Card(cardId, walletId, currency, 0, number, expirationDate, ccv, userId, isBlocked = false)).pure[Q]
      }
    }

  def loadCard(userId: UserId, cardId: String, amount: BigDecimal Refined Positive): F[LoadCardCommandValidation] =
    Logger[F].info(s"Load card $cardId") *> transact {
      repository.queryCard(cardId).flatMap {
        case None => LoadCardCommandValidation.cardUnknown(cardId).pure[Q]
        case Some((cardId, ownerId, _, _, _, _)) if ownerId != userId => LoadCardCommandValidation.notCardOwner(userId, cardId).pure[Q]
        case Some((cardId, _, _, _, _, true)) => LoadCardCommandValidation.cardBlocked(cardId).pure[Q]
        case Some((cardId, _, walletId, cardBalance, currency, false)) =>
          repository.queryWalletBalance(walletId).flatMap {
            case walletBalance if walletBalance < amount.value => LoadCardCommandValidation.walletBalanceTooLow(walletId, walletBalance).pure[Q]
            case walletBalance =>
              val newWalletBalance = walletBalance - amount.value
              val newCardBalance = cardBalance + amount.value
              repository.setWalletBalance(walletId)(newWalletBalance) *>
                repository.setCardBalance(cardId)(newCardBalance) *>
                repository.setTransfer(TransferId(UUID.randomUUID()), LocalDateTime.now(), amount.value, currency, currency, Option.empty, WalletEntity(walletId), CardEntity(cardId)) *>
                LoadCardCommandValidation.cardCredited(cardId, newCardBalance).pure[Q]
          }
      }
    }

  def blockCard(userId: UserId, cardId: String): F[BlockCardCommandValidation] =
    Logger[F].info(s"Block card $cardId") *> transact {
      repository.queryCard(cardId).flatMap {
        case None => BlockCardCommandValidation.cardUnknown(cardId).pure[Q]
        case Some((cardId, ownerId, _, _, _, _)) if ownerId != userId => BlockCardCommandValidation.notCardOwner(userId, cardId).pure[Q]
        case Some((cardId, _, _, _, _, true)) => BlockCardCommandValidation.cardAlreadyBlocked(cardId).pure[Q]
        case Some((cardId, _, walletId, cardBalance, currency, false)) => repository.blockCard(cardId) *>
          repository.setCardBalance(cardId)(0) *>
          repository.queryWalletBalance(walletId).flatMap(walletBalance => repository.setWalletBalance(walletId)(walletBalance + cardBalance)) *>
          repository.setTransfer(TransferId(UUID.randomUUID()), LocalDateTime.now(), cardBalance, currency, currency, Option.empty, CardEntity(cardId), WalletEntity(walletId)) *>
          BlockCardCommandValidation.cardBlocked(cardId).pure[Q]
      }
    }

  def unblockCard(userId: UserId, cardId: String): F[UnblockCardCommandValidation] =
    Logger[F].info(s"Unblock card $cardId") *> transact {
      repository.queryCard(cardId).flatMap {
        case None => UnblockCardCommandValidation.cardUnknown(cardId).pure[Q]
        case Some((cardId, ownerId, _, _, _, _)) if ownerId != userId => UnblockCardCommandValidation.notCardOwner(userId, cardId).pure[Q]
        case Some((cardId, _, _, _, _, false)) => UnblockCardCommandValidation.cardAlreadyUnblocked(cardId).pure[Q]
        case Some((cardId, _, _, _, _, true)) => repository.unblockCard(cardId) *> UnblockCardCommandValidation.cardUnblocked(cardId).pure[Q]
      }
    }

  def transfer(companyId: CompanyId)(amount: BigDecimal Refined Positive, source: WalletId, target: WalletId): F[TransferCommandValidation] =
    Logger[F].info(s"Transfer between $source and $target") *> transact {
      repository.queryWallet(companyId, source).flatMap {
        case None => TransferCommandValidation.notWalletOwner(source).pure[Q]
        case Some((sourceId, sourceBalance, _)) if sourceBalance < amount.value => TransferCommandValidation.walletBalanceTooLow(sourceId, sourceBalance).pure[Q]
        case Some((sourceId, sourceBalance, sourceCurrency)) =>
          repository.queryWallet(companyId, target).flatMap {
            case None => TransferCommandValidation.walletUnknown(target).pure[Q]
            case Some((targetId, targetBalance, targetCurrency)) if sourceCurrency == targetCurrency =>
              val transferId = TransferId(UUID.randomUUID())
              val timestamp = LocalDateTime.now()
              repository.setWalletBalance(sourceId)(sourceBalance - amount.value) *>
                repository.setWalletBalance(targetId)(targetBalance + amount.value) *>
                repository.setTransfer(transferId, timestamp, amount.value, sourceCurrency, targetCurrency, Option.empty, WalletEntity(sourceId), WalletEntity(targetId)) *>
                TransferCommandValidation.transfered(Transfer(transferId, timestamp, amount.value, sourceCurrency, targetCurrency, Option.empty, WalletEntity(sourceId), WalletEntity(targetId))).pure[Q]
            case Some((targetId, targetBalance, targetCurrency)) =>
              val transferId = TransferId(UUID.randomUUID())
              val timestamp = LocalDateTime.now()
              val exchange = CurrencyExchange((sourceCurrency, targetCurrency))
              val amountWithExchange = amount.value * exchange
              val fee = (Fee / 100) * amountWithExchange
              val amountToCredit = amountWithExchange - fee
              repository.setWalletBalance(sourceId)(sourceBalance - amount.value) *>
                repository.setWalletBalance(targetId)(targetBalance + amountToCredit) *>
                repository.setTransfer(transferId, timestamp, amount.value, sourceCurrency, targetCurrency, fee.some, WalletEntity(sourceId), WalletEntity(targetId)) *>
                repository.queryMasterWallet(targetCurrency).flatMap {
                  case (masterWalletId, masterWalletBalance) =>
                    repository.setWalletBalance(masterWalletId)(masterWalletBalance + fee) *>
                      repository.setTransfer(TransferId(UUID.randomUUID()), timestamp, fee, targetCurrency, targetCurrency, Option.empty, WalletEntity(targetId), WalletEntity(masterWalletId))
                } *>
                TransferCommandValidation.transfered(Transfer(transferId, timestamp, amount.value, sourceCurrency, targetCurrency, fee.some, WalletEntity(sourceId), WalletEntity(targetId))).pure[Q]
          }
      }
    }

}
