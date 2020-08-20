package repository

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import cats.effect.Sync
import cats.{Monad, ~>}
import doobie.implicits._
import doobie.implicits.javatime._
import doobie.{ConnectionIO, Transactor}
import model.Card.CardId
import model.Company.CompanyId
import model.Transfer.{TransferEntity, TransferId}
import model.User.UserId
import model.Wallet.WalletId
import model.commands.Credentials
import model.{Card, Company, Currency, User, Wallet, _}
import doobie.refined.implicits._

class SQLBankingRepository[F[_] : Sync](transactor: Transactor[F]) extends BankingRepository[ConnectionIO, F] {

  override implicit val Transform: ConnectionIO ~> F = new (ConnectionIO ~> F) {
    override def apply[A](fa: ConnectionIO[A]): F[A] = transactor.trans.apply(fa)
  }

  implicit val Instance: Monad[ConnectionIO] = implicitly

  def authenticate(credentials: Credentials): ConnectionIO[Option[(UserId, CompanyId)]] = {
    sql"SELECT U.ID, C.ID FROM USERS U JOIN COMPANIES C on U.COMPANY_ID = C.ID WHERE U.ID = ${credentials.userId.value} AND C.ID = ${credentials.companyId.value}"
      .query[(UserId, CompanyId)]
      .option
  }

  def listCompanies: ConnectionIO[List[Company]] = {
    sql"SELECT ID, NAME FROM COMPANIES"
      .query[Company]
      .to[List]
  }

  def listUsers: ConnectionIO[List[User]] = {
    sql"SELECT ID, COMPANY_ID FROM USERS"
      .query[User]
      .to[List]
  }

  def listCards(userId: UserId): ConnectionIO[List[Card]] = {
    sql"SELECT ID, WALLET_ID, CURRENCY, BALANCE, NUMBER, EXPIRATION_DATE, CCV, USER_ID, IS_BLOCKED FROM cards WHERE USER_ID = $userId"
      .query[Card]
      .to[List]
  }

  def listWallets(companyId: CompanyId): ConnectionIO[List[Wallet]] = {
    sql"SELECT ID, BALANCE, CURRENCY, COMPANY_ID, IS_MASTER FROM wallets WHERE COMPANY_ID = $companyId"
      .query[Wallet]
      .to[List]
  }

  def createWallet(id: UUID)(companyId: CompanyId)(balance: BigDecimal, currency: Currency, isMaster: Boolean) = {
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
    sql"SELECT W.ID, W.BALANCE FROM WALLETS W JOIN COMPANIES C on C.ID = W.COMPANY_ID WHERE CURRENCY = $currency AND C.NAME = 'Holding'"
      .query[(WalletId, BigDecimal)]
      .unique

  def createCard(currency: Currency)(cardId: CardId, number: String, expirationDate: LocalDate, ccv: String)(userId: UserId)(walletId: WalletId) =
    sql"INSERT INTO CARDS (ID, WALLET_ID, CURRENCY, BALANCE, NUMBER, EXPIRATION_DATE, CCV, USER_ID, IS_BLOCKED) VALUES ($cardId, $walletId, $currency, 0, $number, $expirationDate, $ccv, $userId, false)"
      .update
      .run

  def queryCard(cardId: CardId) =
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