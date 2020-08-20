package repository

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import cats.{Monad, ~>}
import model.Card.CardId
import model.Company.CompanyId
import model.Transfer.{TransferEntity, TransferId}
import model.User.UserId
import model.Wallet.WalletId
import model._
import model.commands._

sealed trait AuthenticationStatus

trait BankingRepository[Q[_], F[_]] {

  implicit val Instance: Monad[Q]

  implicit val Transform: Q ~> F

  def transact[A](query: Q[A])(implicit T: Q ~> F): F[A] = T(query)

  def authenticate(credentials: Credentials): Q[Option[(UserId, CompanyId)]]

  def listCompanies: Q[List[Company]]

  def listUsers: Q[List[User]]

  def listCards(userId: UserId): Q[List[Card]]

  def listWallets(companyId: CompanyId): Q[List[Wallet]]

  def createWallet(id: UUID)(companyId: CompanyId)(balance: BigDecimal, currency: Currency, isMaster: Boolean): Q[Wallet]

  def queryWallet(companyId: CompanyId, walletId: WalletId): Q[Option[(WalletId, BigDecimal, Currency)]]

  def queryMasterWallet(currency: Currency): Q[(WalletId, BigDecimal)]

  def createCard(currency: Currency)(cardId: CardId, number: String, expirationDate: LocalDate, ccv: String)(userId: UserId)(walletId: WalletId): Q[Int]

  def queryCard(cardId: CardId): Q[Option[(CardId, UserId, WalletId, BigDecimal, Currency, Boolean)]]

  def queryWalletBalance(walletId: WalletId): Q[BigDecimal]

  def setWalletBalance(walletId: WalletId)(balance: BigDecimal): Q[Int]

  def setCardBalance(cardId: CardId)(balance: BigDecimal): Q[Int]

  def setTransfer(id: TransferId, timestamp: LocalDateTime, amount: BigDecimal, sourceCurrency: Currency, targetCurrency: Currency, fees: Option[BigDecimal], source: TransferEntity, target: TransferEntity): Q[Int]

  def blockCard(cardId: CardId): Q[Int]

  def unblockCard(cardId: CardId): Q[Int]
}
