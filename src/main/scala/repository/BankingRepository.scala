package repository

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import cats.Applicative.ops.toAllApplicativeOps
import cats.effect.IO
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
import model.Transfer.{TransferEntity, TransferId}
import model.User.UserId
import model.Wallet.WalletId
import model._
import model.commands._
import repository.AuthenticationStatus.{Authenticated, NotAllowed}

sealed trait AuthenticationStatus

object AuthenticationStatus {

  final case class Authenticated(userId: UserId, companyId: CompanyId) extends AuthenticationStatus

  final case object NotAllowed extends AuthenticationStatus

}

class BankingRepository(transactor: Transactor[IO]) {

  private val SpendeskFee = 2.9

  private val CurrencyExchange: Map[(Currency, Currency), BigDecimal] = Map(
    (EUR, USD) -> 1.18,
    (EUR, GBP) -> 0.90,
    (USD, EUR) -> 0.85,
    (USD, GBP) -> 0.77,
    (GBP, EUR) -> 1.11,
    (GBP, USD) -> 1.30
  )

  def authenticate(credentials: Credentials): IO[AuthenticationStatus] = {
    sql"SELECT U.ID, C.ID FROM USERS U JOIN COMPANIES C on U.COMPANY_ID = C.ID WHERE U.ID = ${credentials.userId.value} AND C.ID = ${credentials.companyId.value}"
      .query[(UserId, CompanyId)]
      .option.map {
      case Some((userId, companyId)) => Authenticated(userId, companyId)
      case None => NotAllowed
    }
      .transact(transactor)
  }

  def listCompanies: Stream[IO, Company] = {
    sql"SELECT ID, NAME FROM COMPANIES"
      .query[Company]
      .stream
      .transact(transactor)
  }

  def listUsers: Stream[IO, User] = {
    sql"SELECT ID, COMPANY_ID FROM USERS"
      .query[User]
      .stream
      .transact(transactor)
  }

  def listCards(userId: UserId): Stream[IO, Card] = {
    sql"SELECT ID, WALLET_ID, CURRENCY, BALANCE, NUMBER, EXPIRATION_DATE, CCV, USER_ID, IS_BLOCKED FROM cards WHERE USER_ID = $userId"
      .query[Card]
      .stream
      .transact(transactor)
  }

  def listWallets(companyId: CompanyId): Stream[IO, Wallet] = {
    sql"SELECT ID, BALANCE, CURRENCY, COMPANY_ID, IS_MASTER FROM wallets WHERE COMPANY_ID = $companyId"
      .query[Wallet]
      .stream
      .transact(transactor)
  }

  def createWallet(id: UUID)(companyId: CompanyId)(command: CreateWalletCommand): IO[Wallet] = {
    sql"INSERT INTO WALLETS (ID, BALANCE, CURRENCY, COMPANY_ID, IS_MASTER) VALUES ($id, ${command.balance}, ${command.currency}, $companyId, ${command.isMaster})"
      .update
      .run
      .transact(transactor)
      .map { _ =>
        Wallet(WalletId(id), command.balance, command.currency, companyId, command.isMaster)
      }
  }

  def createCard(id: UUID, number: String, expirationDate: LocalDate, ccv: String)(userId: UserId, companyId: CompanyId)(command: CreateCardCommand): IO[CreateCardCommandValidation] = {
    val queryWallet = sql"SELECT ID, CURRENCY FROM WALLETS WHERE COMPANY_ID = $companyId AND ID = ${command.walletId}".query[(WalletId, Currency)].option

    def createCard(currency: Currency) = sql"INSERT INTO CARDS (ID, WALLET_ID, CURRENCY, BALANCE, NUMBER, EXPIRATION_DATE, CCV, USER_ID, IS_BLOCKED) VALUES ($id, ${command.walletId}, $currency, 0, $number, $expirationDate, $ccv, $userId, false)".update.run

    queryWallet.flatMap {
      case None => CreateCardCommandValidation.notWalletOwner(command.walletId).pure[ConnectionIO]
      case Some((walletId, currency)) => createCard(currency) *> CreateCardCommandValidation.cardCreated(Card(CardId(id), walletId, currency, 0, number, expirationDate, ccv, userId, isBlocked = false)).pure[ConnectionIO]
    }.transact(transactor)

  }

  private def queryCard(cardId: String) = sql"SELECT ID, USER_ID, WALLET_ID, BALANCE, CURRENCY, IS_BLOCKED FROM CARDS WHERE ID = $cardId".query[(CardId, UserId, WalletId, BigDecimal, Currency, Boolean)].option

  private def queryWalletBalance(walletId: WalletId) = sql"SELECT BALANCE FROM WALLETS WHERE ID = $walletId".query[BigDecimal].unique

  private def setWalletBalance(walletId: WalletId)(balance: BigDecimal) = sql"UPDATE WALLETS SET BALANCE = $balance WHERE ID = $walletId".update.run

  private def setCardBalance(cardId: CardId)(balance: BigDecimal) = sql"UPDATE CARDS SET BALANCE = $balance WHERE ID = $cardId".update.run

  private def setTransfer(id: TransferId, timestamp: LocalDateTime, amount: BigDecimal, sourceCurrency: Currency, targetCurrency: Currency, fees: Option[BigDecimal], source: TransferEntity, target: TransferEntity) =
    sql"INSERT INTO TRANSFERS(ID, TIMESTAMP, AMOUNT, ORIGIN_CURRENCY, TARGET_CURRENCY, CONVERSION_FEE, ORIGIN_ENTITY_ID, ORIGIN_ENTITY_TYPE, TARGET_ENTITY_ID, TARGET_ENTITY_TYPE) VALUES ( $id, $timestamp, $amount, $sourceCurrency, $targetCurrency, $fees, ${source.id}, ${source.entity}, ${target.id}, ${target.entity} )".update.run

  def loadCard(userId: UserId, cardId: String, amount: BigDecimal): IO[LoadCardCommandValidation] = {
    queryCard(cardId).flatMap {
      case None => LoadCardCommandValidation.cardUnknown(cardId).pure[ConnectionIO]
      case Some((cardId, ownerId, _, _, _, _)) if ownerId != userId => LoadCardCommandValidation.notCardOwner(userId, cardId).pure[ConnectionIO]
      case Some((cardId, _, _, _, _, true)) => LoadCardCommandValidation.cardBlocked(cardId).pure[ConnectionIO]
      case Some((cardId, _, walletId, cardBalance, currency, false)) =>
        queryWalletBalance(walletId).flatMap {
          case walletBalance if walletBalance < amount => LoadCardCommandValidation.walletBalanceTooLow(walletId, walletBalance).pure[ConnectionIO]
          case walletBalance =>
            val newWalletBalance = walletBalance - amount
            val newCardBalance = cardBalance + amount
            setWalletBalance(walletId)(newWalletBalance) *>
              setCardBalance(cardId)(newCardBalance) *>
              setTransfer(TransferId(UUID.randomUUID()), LocalDateTime.now(), amount, currency, currency, Option.empty, WalletEntity(walletId), CardEntity(cardId)) *>
              LoadCardCommandValidation.cardCredited(cardId, newCardBalance).pure[ConnectionIO]
        }
    }
  }.transact(transactor)

  def blockCard(userId: UserId, cardId: String): IO[BlockCardCommandValidation] = {
    val blockCard = sql"UPDATE CARDS SET IS_BLOCKED = true WHERE ID = $cardId".update.run

    queryCard(cardId).flatMap {
      case None => BlockCardCommandValidation.cardUnknown(cardId).pure[ConnectionIO]
      case Some((cardId, ownerId, _, _, _, _)) if ownerId != userId => BlockCardCommandValidation.notCardOwner(userId, cardId).pure[ConnectionIO]
      case Some((cardId, _, _, _, _, true)) => BlockCardCommandValidation.cardAlreadyBlocked(cardId).pure[ConnectionIO]
      case Some((cardId, _, walletId, cardBalance, currency, false)) => blockCard *>
        setCardBalance(cardId)(0) *>
        queryWalletBalance(walletId).flatMap(walletBalance => setWalletBalance(walletId)(walletBalance + cardBalance)) *>
        setTransfer(TransferId(UUID.randomUUID()), LocalDateTime.now(), cardBalance, currency, currency, Option.empty, CardEntity(cardId), WalletEntity(walletId)) *>
        BlockCardCommandValidation.cardBlocked(cardId).pure[ConnectionIO]
    }
  }.transact(transactor)

  def unblockCard(userId: UserId, cardId: String): IO[UnblockCardCommandValidation] = {
    val queryCard = sql"SELECT ID, USER_ID, IS_BLOCKED FROM CARDS WHERE ID = $cardId".query[(CardId, UserId, Boolean)].option
    val unblockCard = sql"UPDATE CARDS SET IS_BLOCKED = false WHERE ID = $cardId".update.run

    queryCard.flatMap {
      case None => UnblockCardCommandValidation.cardUnknown(cardId).pure[ConnectionIO]
      case Some((cardId, ownerId, _)) if ownerId != userId => UnblockCardCommandValidation.notCardOwner(userId, cardId).pure[ConnectionIO]
      case Some((cardId, _, false)) => UnblockCardCommandValidation.cardAlreadyUnblocked(cardId).pure[ConnectionIO]
      case Some((cardId, _, true)) => unblockCard *> UnblockCardCommandValidation.cardUnblocked(cardId).pure[ConnectionIO]
    }
  }.transact(transactor)

  def transfer(companyId: CompanyId)(amount: BigDecimal, source: WalletId, target: WalletId): IO[TransferCommandValidation] = {
    val querySource = sql"SELECT ID, BALANCE, CURRENCY FROM WALLETS WHERE ID = $source AND COMPANY_ID = $companyId".query[(WalletId, BigDecimal, Currency)].option
    val queryTarget = sql"SELECT ID, BALANCE, CURRENCY FROM WALLETS WHERE ID = $target".query[(WalletId, BigDecimal, Currency)].option

    def queryMasterWallet(currency: Currency) = sql"SELECT W.ID, W.BALANCE FROM WALLETS W JOIN COMPANIES C on C.ID = W.COMPANY_ID WHERE CURRENCY = $currency AND C.NAME = 'Spendesk'".query[(WalletId, BigDecimal)].unique

    querySource.flatMap {
      case None => TransferCommandValidation.notWalletOwner(source).pure[ConnectionIO]
      case Some((sourceId, sourceBalance, _)) if sourceBalance < amount => TransferCommandValidation.walletBalanceTooLow(sourceId, sourceBalance).pure[ConnectionIO]
      case Some((sourceId, sourceBalance, sourceCurrency)) =>
        queryTarget.flatMap {
          case None => TransferCommandValidation.walletUnknown(target).pure[ConnectionIO]
          case Some((targetId, targetBalance, targetCurrency)) if sourceCurrency == targetCurrency =>
            val transferId = TransferId(UUID.randomUUID())
            val timestamp = LocalDateTime.now()
            setWalletBalance(sourceId)(sourceBalance - amount) *>
              setWalletBalance(targetId)(targetBalance + amount) *>
              setTransfer(transferId, timestamp, amount, sourceCurrency, targetCurrency, Option.empty, WalletEntity(sourceId), WalletEntity(targetId)) *>
              TransferCommandValidation.transfered(Transfer(transferId, timestamp, amount, sourceCurrency, targetCurrency, Option.empty, WalletEntity(sourceId), WalletEntity(targetId))).pure[ConnectionIO]
          case Some((targetId, targetBalance, targetCurrency)) =>
            val transferId = TransferId(UUID.randomUUID())
            val timestamp = LocalDateTime.now()
            val exchange = CurrencyExchange((sourceCurrency, targetCurrency))
            val amountWithExchange = amount * exchange
            val spendeskFee = (SpendeskFee / 100) * amountWithExchange
            val amountToCredit = amountWithExchange - spendeskFee
            setWalletBalance(sourceId)(sourceBalance - amount) *>
              setWalletBalance(targetId)(targetBalance + amountToCredit) *>
              setTransfer(transferId, timestamp, amount, sourceCurrency, targetCurrency, spendeskFee.some, WalletEntity(sourceId), WalletEntity(targetId)) *>
              queryMasterWallet(targetCurrency).flatMap {
                case (masterWalletId, masterWalletBalance) =>
                  setWalletBalance(masterWalletId)(masterWalletBalance + spendeskFee) *>
                    setTransfer(TransferId(UUID.randomUUID()), timestamp, spendeskFee, targetCurrency, targetCurrency, Option.empty, WalletEntity(targetId), WalletEntity(masterWalletId))
              } *>
              TransferCommandValidation.transfered(Transfer(transferId, timestamp, amount, sourceCurrency, targetCurrency, spendeskFee.some, WalletEntity(sourceId), WalletEntity(targetId))).pure[ConnectionIO]
        }
    }
  }.transact(transactor)

}
