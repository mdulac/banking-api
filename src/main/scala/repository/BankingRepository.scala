package repository

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import cats.{Monad, ~>}
import doobie.implicits.javatime._
import model.Card.CardId
import model.Company.CompanyId
import model.Transfer.{TransferEntity, TransferId}
import model.User.UserId
import model.Wallet.WalletId
import model._
import model.commands._

sealed trait AuthenticationStatus

trait BankingRepository[Query[_], F[_]] {

  implicit val Instance: Monad[Query]

  implicit val Transform: Query ~> F

  def transact[A](query: Query[A])(implicit T: Query ~> F): F[A] = T(query)

  def authenticate(credentials: Credentials): Query[Option[(UserId, CompanyId)]]

  def listCompanies: Query[List[Company]]

  def listUsers: Query[List[User]]

  def listCards(userId: UserId): Query[List[Card]]

  def listWallets(companyId: CompanyId): Query[List[Wallet]]

  def createWallet(id: UUID)(companyId: CompanyId)(balance: BigDecimal, currency: Currency, isMaster: Boolean): Query[Wallet]

  def queryWallet(companyId: CompanyId, walletId: WalletId): Query[Option[(WalletId, BigDecimal, Currency)]]

  def queryMasterWallet(currency: Currency): Query[(WalletId, BigDecimal)]

  def createCard(currency: Currency)(cardId: CardId, number: String, expirationDate: LocalDate, ccv: String)(userId: UserId)(walletId: WalletId): Query[Int]

  def queryCard(cardId: String): Query[Option[(CardId, UserId, WalletId, BigDecimal, Currency, Boolean)]]

  def queryWalletBalance(walletId: WalletId): Query[BigDecimal]

  def setWalletBalance(walletId: WalletId)(balance: BigDecimal): Query[Int]

  def setCardBalance(cardId: CardId)(balance: BigDecimal): Query[Int]

  def setTransfer(id: TransferId, timestamp: LocalDateTime, amount: BigDecimal, sourceCurrency: Currency, targetCurrency: Currency, fees: Option[BigDecimal], source: TransferEntity, target: TransferEntity): Query[Int]

  def blockCard(cardId: CardId): Query[Int]

  def unblockCard(cardId: CardId): Query[Int]
}
