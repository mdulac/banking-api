package services

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID
import java.util.UUID.fromString

import cats.Applicative.ops.toAllApplicativeOps
import cats.effect.IO
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxOptionId}
import cats.{Monad, ~>}
import model.Card.CardId
import model.Company.CompanyId
import model.User.UserId
import model.Wallet.WalletId
import model.commands.{Credentials, LoadCardCommandValidation}
import model.{Card, Company, Currency, Transfer, User, Wallet}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import repository.BankingRepository

class BankingServicesSpec extends AnyFlatSpec with Matchers {

  def newRepository: BankingRepository[IO, IO] = new BankingRepository[IO, IO]() {

    def modify[A](list: List[A])(f: A => Boolean)(m: A => A): List[A] = {
      list.find(f) match {
        case None => list
        case Some(el) => m(el) +: list.filter(a => !f(a))
      }
    }

    override implicit val Transform: IO ~> IO = new (IO ~> IO) {
      override def apply[A](fa: IO[A]): IO[A] = fa
    }

    override val Instance: Monad[IO] = implicitly

    override def authenticate(credentials: Credentials): IO[Option[(UserId, CompanyId)]] =
      (UserId(fromString("c117d7e1-b746-4190-99cd-91d8c91211ec")), CompanyId(fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17"))).some.pure[IO]

    val companies = List(
      Company(CompanyId(fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17")), "Google"),
    )

    val users = List(
      User(UserId(fromString("c117d7e1-b746-4190-99cd-91d8c91211ec")), CompanyId(fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17"))),
    )

    var cards = List(
      Card(
        CardId(fromString("ac7c35f7-fb14-4df6-b006-2f18d50268a4")),
        WalletId(fromString("cb9ef80a-22e9-434a-92bc-0bf060b6ef31")),
        Currency.EUR,
        0,
        "1111111111111111",
        LocalDate.now(),
        "123",
        UserId(fromString("c117d7e1-b746-4190-99cd-91d8c91211ec")),
        isBlocked = false
      )
    )

    var wallets = List(
      Wallet(
        WalletId(fromString("cb9ef80a-22e9-434a-92bc-0bf060b6ef31")),
        0,
        Currency.EUR,
        CompanyId(fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17")),
        isMaster = false
      )
    )

    var transfers = List.empty[Transfer]

    override def listCompanies: IO[List[Company]] = companies.pure[IO]

    override def listUsers: IO[List[User]] = users.pure[IO]

    override def listCards(userId: User.UserId): IO[List[Card]] = cards.pure[IO]

    override def listWallets(companyId: Company.CompanyId): IO[List[Wallet]] = wallets.pure[IO]

    override def createWallet(id: UUID)(companyId: Company.CompanyId)(balance: BigDecimal, currency: Currency, isMaster: Boolean): IO[Wallet] = {
      val wallet = Wallet(WalletId(id), balance, currency, companyId, isMaster)
      wallets = wallet +: wallets
      wallet.pure[IO]
    }

    override def queryWallet(companyId: Company.CompanyId, walletId: Wallet.WalletId): IO[Option[(Wallet.WalletId, BigDecimal, Currency)]] =
      wallets.find(w => w.companyId == companyId && w.walletId == walletId).map(w => (w.walletId, w.balance, w.currency)).pure[IO]

    override def queryMasterWallet(currency: Currency): IO[(Wallet.WalletId, BigDecimal)] = ??? // FIXME

    override def createCard(currency: Currency)(id: UUID, number: String, expirationDate: LocalDate, ccv: String)(userId: User.UserId)(walletId: Wallet.WalletId): IO[Int] = {
      val card = Card(CardId(id), walletId, currency, 0, number, expirationDate, ccv, userId, isBlocked = false)
      cards = card +: cards
      1.pure[IO]
    }

    override def queryCard(cardId: String): IO[Option[(Card.CardId, User.UserId, Wallet.WalletId, BigDecimal, Currency, Boolean)]] =
      cards.find(c => c.cardId.toString == cardId).map(c => (c.cardId, c.userId, c.walletId, c.balance, c.currency, c.isBlocked)).pure[IO]

    override def queryWalletBalance(walletId: Wallet.WalletId): IO[BigDecimal] = wallets.find(w => w.walletId == walletId).map(_.balance).get.pure[IO] // FIXME

    override def setWalletBalance(walletId: Wallet.WalletId)(balance: BigDecimal): IO[Int] = {
      val newWallets = modify(wallets)(w => w.walletId == walletId)(_.copy(balance = balance))
      wallets = newWallets
      1.pure[IO]
    }

    override def setCardBalance(cardId: Card.CardId)(balance: BigDecimal): IO[Int] = {
      val newCards = modify(cards)(c => c.cardId == cardId)(_.copy(balance = balance))
      cards = newCards
      1.pure[IO]
    }

    override def setTransfer(id: Transfer.TransferId, timestamp: LocalDateTime, amount: BigDecimal, sourceCurrency: Currency, targetCurrency: Currency, fees: Option[BigDecimal], source: Transfer.TransferEntity, target: Transfer.TransferEntity): IO[Int] = {
      val transfer = Transfer(id, timestamp, amount, sourceCurrency, targetCurrency, fees, source, target)
      transfers = transfer +: transfers
      1.pure[IO]
    }

    override def blockCard(cardId: Card.CardId): IO[Int] = {
      val newCards = modify(cards)(c => c.cardId == cardId)(_.copy(isBlocked = true))
      cards = newCards
      1.pure[IO]
    }

    override def unblockCard(cardId: Card.CardId): IO[Int] = {
      val newCards = modify(cards)(c => c.cardId == cardId)(_.copy(isBlocked = false))
      cards = newCards
      1.pure[IO]
    }

  }

  "Service" should "not load an unknown card" in {
    val unknownCardId = "ac7c35f7-fb14-4df6-b006-2f18d50268a6"
    val service = new BankingService(newRepository)

    val p = service.loadCard(UserId(fromString("c117d7e1-b746-4190-99cd-91d8c91211ec")), unknownCardId, 1)

    p.unsafeRunSync() match {
      case LoadCardCommandValidation.CardUnknown(_) => succeed
      case f => fail(s"Should be Card Unknown : ${f}")
    }
  }

  it should "not load a blocked card" in {
    val service = new BankingService(newRepository)
    val userId = UserId(fromString("c117d7e1-b746-4190-99cd-91d8c91211ec"))

    val p =
      service.blockCard(userId, "ac7c35f7-fb14-4df6-b006-2f18d50268a4") *>
        service.loadCard(userId, "ac7c35f7-fb14-4df6-b006-2f18d50268a4", 1)

    p.unsafeRunSync() match {
      case LoadCardCommandValidation.CardBlocked(_) => succeed
      case f => fail(s"Should be Card Blocked : ${f}")
    }
  }

}
