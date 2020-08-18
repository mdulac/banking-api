package services

import java.time.LocalDate.now
import java.time.{LocalDate, LocalDateTime}
import java.util.UUID
import java.util.UUID.fromString

import cats.effect.IO
import cats.implicits.catsSyntaxOptionId
import cats.{Id, Monad, ~>}
import model.Card.CardId
import model.Card.CardId.CardIdOps
import model.Company.CompanyId
import model.Company.CompanyId.CompanyIdOps
import model.Currency._
import model.Transfer.{TransferEntity, TransferId}
import model.User.UserId
import model.User.UserId.UserIdOps
import model.Wallet.WalletId
import model.Wallet.WalletId.WalletIdOps
import model.commands.CreateCardCommandValidation.CardCreated
import model.commands.{CreateCardCommand, CreateWalletCommand, Credentials, LoadCardCommandValidation}
import model.{Card, Company, Currency, Transfer, User, Wallet}
import org.scalacheck.Gen
import org.scalacheck.Gen.{choose, uuid}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import repository.BankingRepository


class BankingServiceSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  def newRepository(): BankingRepository[Id, IO] = new BankingRepository[Id, IO]() {

    private def modify[A](list: List[A])(f: A => Boolean)(m: A => A): List[A] = {
      list.find(f) match {
        case None => list
        case Some(el) => m(el) +: list.filter(a => !f(a))
      }
    }

    override implicit val Transform: Id ~> IO = new (Id ~> IO) {
      override def apply[A](fa: Id[A]): IO[A] = IO.delay(fa)
    }

    override val Instance: Monad[Id] = implicitly[Monad[Id]]

    override def authenticate(credentials: Credentials): Id[Option[(UserId, CompanyId)]] =
      (fromString("c117d7e1-b746-4190-99cd-91d8c91211ec").userId, fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId).some

    val companies = List(
      Company(fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId, "Google")
    )

    val users = List(
      User(fromString("c117d7e1-b746-4190-99cd-91d8c91211ec").userId, fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId)
    )

    var cards = List.empty[Card]

    var wallets = List.empty[Wallet]

    var transfers = List.empty[Transfer]

    override def listCompanies: Id[List[Company]] = companies

    override def listUsers: Id[List[User]] = users

    override def listCards(userId: UserId): Id[List[Card]] = cards.filter(_.userId == userId)

    override def listWallets(companyId: CompanyId): Id[List[Wallet]] = wallets.filter(_.companyId == companyId)

    override def createWallet(id: UUID)(companyId: CompanyId)(balance: BigDecimal, currency: Currency, isMaster: Boolean): Id[Wallet] = {
      val wallet = Wallet(id.walletId, balance, currency, companyId, isMaster)
      wallets = wallet +: wallets
      wallet
    }

    override def queryWallet(companyId: CompanyId, walletId: WalletId): Id[Option[(WalletId, BigDecimal, Currency)]] =
      wallets.find(w => w.companyId == companyId && w.walletId == walletId).map(w => (w.walletId, w.balance, w.currency))

    override def queryMasterWallet(currency: Currency): Id[(WalletId, BigDecimal)] = ??? // FIXME

    override def createCard(currency: Currency)(cardId: CardId, number: String, expirationDate: LocalDate, ccv: String)(userId: UserId)(walletId: WalletId): Id[Int] = {
      val card = Card(cardId, walletId, currency, 0, number, expirationDate, ccv, userId, isBlocked = false)
      cards = card +: cards
      1
    }

    override def queryCard(cardId: String): Id[Option[(CardId, UserId, WalletId, BigDecimal, Currency, Boolean)]] =
      cards.find(c => c.cardId.toString == cardId).map(c => (c.cardId, c.userId, c.walletId, c.balance, c.currency, c.isBlocked))

    override def queryWalletBalance(walletId: WalletId): Id[BigDecimal] = wallets.find(w => w.walletId == walletId).map(_.balance).get // FIXME

    override def setWalletBalance(walletId: WalletId)(balance: BigDecimal): Id[Int] = {
      val newWallets = modify(wallets)(w => w.walletId == walletId)(_.copy(balance = balance))
      wallets = newWallets
      1
    }

    override def setCardBalance(cardId: CardId)(balance: BigDecimal): Id[Int] = {
      val newCards = modify(cards)(c => c.cardId == cardId)(_.copy(balance = balance))
      cards = newCards
      1
    }

    override def setTransfer(id: TransferId, timestamp: LocalDateTime, amount: BigDecimal, sourceCurrency: Currency, targetCurrency: Currency, fees: Option[BigDecimal], source: TransferEntity, target: TransferEntity): Id[Int] = {
      val transfer = Transfer(id, timestamp, amount, sourceCurrency, targetCurrency, fees, source, target)
      transfers = transfer +: transfers
      1
    }

    override def blockCard(cardId: CardId): Id[Int] = {
      val newCards = modify(cards)(c => c.cardId == cardId)(_.copy(isBlocked = true))
      cards = newCards
      1
    }

    override def unblockCard(cardId: CardId): Id[Int] = {
      val newCards = modify(cards)(c => c.cardId == cardId)(_.copy(isBlocked = false))
      cards = newCards
      1
    }

  }

  "A user" should "create a wallet" in {
    forAll(uuid, choose(1, 10), Gen.oneOf(EUR, GBP, USD), Gen.oneOf(true, false)) { (walletId, balance, currency, isMaster) =>
      val companyId = fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId

      val repository = newRepository()
      val service = new BankingService(repository)

      service.createWallet(walletId)(companyId)(CreateWalletCommand(balance, currency, isMaster)).unsafeRunSync() match {
        case Wallet(i, b, c, ci, m) =>
          assert(i == WalletId(walletId))
          assert(b == balance)
          assert(c == currency)
          assert(ci == companyId)
          assert(m == isMaster)
      }
    }
  }

  it should "create a card" in {
    val repository = newRepository()
    val service = new BankingService(repository)

    val cardId = fromString("ac7c35f7-fb14-4df6-b006-2f18d50268a4").cardId
    val userId = fromString("c117d7e1-b746-4190-99cd-91d8c91211ec").userId
    val companyId = fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId
    val walletId = fromString("cb9ef80a-22e9-434a-92bc-0bf060b6ef31").walletId
    val number = "1111111111111111"
    val expirationDate = LocalDate.now()

    repository.createWallet(fromString("cb9ef80a-22e9-434a-92bc-0bf060b6ef31"))(fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId)(1, EUR, isMaster = false)

    service.createCard(cardId, number, expirationDate, "123", userId, companyId)(CreateCardCommand(walletId)).unsafeRunSync() match {
      case CardCreated(_) => succeed
      case f => fail(s"Should be Card Created : $f")
    }
  }

  it should "not load an unknown card" in {
    val unknownCardId = "ac7c35f7-fb14-4df6-b006-2f18d50268a6"
    val repository = newRepository()
    val service = new BankingService(repository)

    val userId = fromString("c117d7e1-b746-4190-99cd-91d8c91211ec").userId
    val walletId = fromString("cb9ef80a-22e9-434a-92bc-0bf060b6ef31").walletId
    val cardId = fromString("ac7c35f7-fb14-4df6-b006-2f18d50268a4").cardId

    repository.createCard(EUR)(cardId, "1111111111111111", now(), "123")(userId)(walletId)

    service.loadCard(userId, unknownCardId, 1).unsafeRunSync() match {
      case LoadCardCommandValidation.CardUnknown(_) => succeed
      case f => fail(s"Should be Card Unknown : $f")
    }
  }

  it should "not load a blocked card" in {
    val repository = newRepository()
    val service = new BankingService(repository)
    import service._

    val userId = fromString("c117d7e1-b746-4190-99cd-91d8c91211ec").userId
    val walletId = fromString("cb9ef80a-22e9-434a-92bc-0bf060b6ef31").walletId
    val cardId = fromString("ac7c35f7-fb14-4df6-b006-2f18d50268a4").cardId

    repository.createCard(EUR)(cardId, "1111111111111111", now(), "123")(userId)(walletId)
    repository.blockCard(cardId)

    loadCard(userId, cardId.toString, 1).unsafeRunSync() match {
      case LoadCardCommandValidation.CardBlocked(_) => succeed
      case f => fail(s"Should be Card Blocked : $f")
    }
  }

}
