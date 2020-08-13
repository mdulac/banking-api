package repository

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import doobie.ConnectionIO
import doobie.implicits._
import doobie.implicits.javatime._
import fs2.Stream
import model.Card.CardId
import model.Company.CompanyId
import model.Transfer.{TransferEntity, TransferId}
import model.User.UserId
import model.Wallet.WalletId
import model._
import model.commands._

sealed trait AuthenticationStatus

class BankingRepository() {

  def authenticate(credentials: Credentials): ConnectionIO[Option[(UserId, CompanyId)]] = {
    sql"SELECT U.ID, C.ID FROM USERS U JOIN COMPANIES C on U.COMPANY_ID = C.ID WHERE U.ID = ${credentials.userId.value} AND C.ID = ${credentials.companyId.value}"
      .query[(UserId, CompanyId)]
      .option
  }

  def listCompanies: Stream[ConnectionIO, Company] = {
    sql"SELECT ID, NAME FROM COMPANIES"
      .query[Company]
      .stream
  }

  def listUsers: Stream[ConnectionIO, User] = {
    sql"SELECT ID, COMPANY_ID FROM USERS"
      .query[User]
      .stream
  }

  def listCards(userId: UserId): Stream[ConnectionIO, Card] = {
    sql"SELECT ID, WALLET_ID, CURRENCY, BALANCE, NUMBER, EXPIRATION_DATE, CCV, USER_ID, IS_BLOCKED FROM cards WHERE USER_ID = $userId"
      .query[Card]
      .stream
  }

  def listWallets(companyId: CompanyId): Stream[ConnectionIO, Wallet] = {
    sql"SELECT ID, BALANCE, CURRENCY, COMPANY_ID, IS_MASTER FROM wallets WHERE COMPANY_ID = $companyId"
      .query[Wallet]
      .stream
  }

  def createWallet(id: UUID)(companyId: CompanyId)(balance: BigDecimal, currency: Currency, isMaster: Boolean): ConnectionIO[Wallet] = {
    sql"INSERT INTO WALLETS (ID, BALANCE, CURRENCY, COMPANY_ID, IS_MASTER) VALUES ($id, $balance, $currency, $companyId, $isMaster)"
      .update
      .run
      .map(_ => Wallet(WalletId(id), balance, currency, companyId, isMaster))
  }

  def queryWallet(companyId: CompanyId, walletId: WalletId) =
    sql"SELECT ID, BALANCE, CURRENCY FROM WALLETS WHERE COMPANY_ID = $companyId AND ID = $walletId"
      .query[(WalletId, BigDecimal, Currency)]
      .option

  def queryMasterWallet(currency: Currency) =
    sql"SELECT W.ID, W.BALANCE FROM WALLETS W JOIN COMPANIES C on C.ID = W.COMPANY_ID WHERE CURRENCY = $currency AND C.NAME = 'Spendesk'"
      .query[(WalletId, BigDecimal)]
      .unique

  def createCard(currency: Currency)(id: UUID, number: String, expirationDate: LocalDate, ccv: String)(userId: UserId)(walletId: WalletId): ConnectionIO[Int] =
    sql"INSERT INTO CARDS (ID, WALLET_ID, CURRENCY, BALANCE, NUMBER, EXPIRATION_DATE, CCV, USER_ID, IS_BLOCKED) VALUES ($id, $walletId, $currency, 0, $number, $expirationDate, $ccv, $userId, false)"
      .update
      .run

  def queryCard(cardId: String) =
    sql"SELECT ID, USER_ID, WALLET_ID, BALANCE, CURRENCY, IS_BLOCKED FROM CARDS WHERE ID = $cardId"
      .query[(CardId, UserId, WalletId, BigDecimal, Currency, Boolean)]
      .option

  def queryWalletBalance(walletId: WalletId) =
    sql"SELECT BALANCE FROM WALLETS WHERE ID = $walletId"
      .query[BigDecimal]
      .unique

  def setWalletBalance(walletId: WalletId)(balance: BigDecimal) =
    sql"UPDATE WALLETS SET BALANCE = $balance WHERE ID = $walletId"
      .update
      .run

  def setCardBalance(cardId: CardId)(balance: BigDecimal) =
    sql"UPDATE CARDS SET BALANCE = $balance WHERE ID = $cardId"
      .update
      .run

  def setTransfer(id: TransferId, timestamp: LocalDateTime, amount: BigDecimal, sourceCurrency: Currency, targetCurrency: Currency, fees: Option[BigDecimal], source: TransferEntity, target: TransferEntity) =
    sql"INSERT INTO TRANSFERS(ID, TIMESTAMP, AMOUNT, ORIGIN_CURRENCY, TARGET_CURRENCY, CONVERSION_FEE, ORIGIN_ENTITY_ID, ORIGIN_ENTITY_TYPE, TARGET_ENTITY_ID, TARGET_ENTITY_TYPE) VALUES ( $id, $timestamp, $amount, $sourceCurrency, $targetCurrency, $fees, ${source.id}, ${source.entity}, ${target.id}, ${target.entity} )"
      .update
      .run

  def blockCard(cardId: CardId) =
    sql"UPDATE CARDS SET IS_BLOCKED = true WHERE ID = $cardId"
      .update
      .run

  def unblockCard(cardId: CardId) =
    sql"UPDATE CARDS SET IS_BLOCKED = false WHERE ID = $cardId"
      .update
      .run

}
