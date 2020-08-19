package services

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID
import java.util.UUID.fromString

import cats.effect.IO
import cats.implicits.catsSyntaxOptionId
import cats.{Id, Monad, ~>}
import eu.timepit.refined
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
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
import org.scalacheck.Gen.uuid
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

  val numberGen: Gen[String] = Gen.listOfN(16, Gen.choose(0, 1)).map(_.mkString(""))
  val ccvGen: Gen[String] = Gen.listOfN(3, Gen.choose(0, 1)).map(_.mkString(""))
  val amountGen: Gen[Int] = Gen.choose(1, 1000)
  val positiveAmountGen: Gen[BigDecimal Refined Positive] =
    Gen.choose(1, 1000)
      .map(BigDecimal.apply)
      .map(refined.refineV[Positive].apply[BigDecimal])
      .flatMap {
        case Right(value) => Gen.const(value)
        case Left(_) => Gen.fail
      }
  val currencyGen: Gen[Currency] = Gen.oneOf(EUR, GBP, USD)
  val booleanGen: Gen[Boolean] = Gen.oneOf(true, false)
  val dateGen: Gen[LocalDate] = Gen.const(LocalDate.now())

  "A user" should "create a wallet" in {
    forAll(uuid, amountGen, currencyGen, booleanGen) { (walletId, balance, currency, isMaster) =>
      val companyId = fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId

      val repository = newRepository()
      val service = new BankingService(repository)

      service.createWallet(walletId)(companyId)(CreateWalletCommand(balance, currency, isMaster)).unsafeRunSync() match {
        case Wallet(i, b, c, ci, m) =>
          i should be(WalletId(walletId))
          b should be(balance)
          c should be(currency)
          ci should be(companyId)
          m should be(isMaster)
      }
    }
  }

  it should "create a card" in {
    forAll(
      Gen.zip(
        Gen.uuid.map(CardId.apply),
        Gen.uuid,
        Gen.const(fromString("c117d7e1-b746-4190-99cd-91d8c91211ec").userId),
        Gen.const(fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId)
      ),
      numberGen,
      ccvGen,
      dateGen
    ) { case ((cardId, walletId, userId, companyId), number, ccv, expirationDate) =>

      val repository = newRepository()
      val service = new BankingService(repository)

      repository.createWallet(walletId)(fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId)(1, EUR, isMaster = false)

      service.createCard(cardId, number, expirationDate, ccv, userId, companyId)(CreateCardCommand(WalletId(walletId))).unsafeRunSync() match {
        case CardCreated(_) => succeed
        case f => fail(s"Should be Card Created : $f")
      }
    }
  }

  it should "load one of its card" in {
    forAll(
      Gen.zip(
        Gen.const(fromString("c117d7e1-b746-4190-99cd-91d8c91211ec").userId),
        Gen.const(fromString("cb9ef80a-22e9-434a-92bc-0bf060b6ef31").walletId),
        Gen.const(fromString("ac7c35f7-fb14-4df6-b006-2f18d50268a4").cardId),
        Gen.const(fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId)
      ),
      positiveAmountGen,
      numberGen,
      ccvGen,
      dateGen
    ) { case ((userId, walletId, cardId, companyId), amount, number, ccv, expirationDate) =>
      val repository = newRepository()
      val service = new BankingService(repository)

      repository.createWallet(walletId.value)(companyId)(amount.value, EUR, isMaster = false)
      repository.createCard(EUR)(cardId, number, expirationDate, ccv)(userId)(walletId)

      service.loadCard(userId, cardId.toString, amount).unsafeRunSync() match {
        case LoadCardCommandValidation.CardCredited(i, b) =>
          i should be(cardId)
          b should be(amount.value)
        case f => fail(s"Should be Card Credited : $f")
      }
    }
  }

  it should "not load one a card if wallet balance is too low" in {
    forAll(
      Gen.zip(
        Gen.const(fromString("c117d7e1-b746-4190-99cd-91d8c91211ec").userId),
        Gen.const(fromString("cb9ef80a-22e9-434a-92bc-0bf060b6ef31").walletId),
        Gen.const(fromString("ac7c35f7-fb14-4df6-b006-2f18d50268a4").cardId),
        Gen.const(fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId)
      ),
      positiveAmountGen,
      numberGen,
      ccvGen,
      dateGen
    ) { case ((userId, walletId, cardId, companyId), amount, number, ccv, expirationDate) =>
      val repository = newRepository()
      val service = new BankingService(repository)

      repository.createWallet(walletId.value)(companyId)(amount.value - 1, EUR, isMaster = false)
      repository.createCard(EUR)(cardId, number, expirationDate, ccv)(userId)(walletId)

      service.loadCard(userId, cardId.toString, amount).unsafeRunSync() match {
        case LoadCardCommandValidation.WalletBalanceTooLow(i, b) =>
          i should be(walletId)
          b should be(amount.value - 1)
        case f => fail(s"Should be Wallet Balance Too Low : $f")
      }
    }
  }

  it should "not load an unknown card" in {
    forAll(
      Gen.zip(
        Gen.uuid,
        Gen.const(fromString("c117d7e1-b746-4190-99cd-91d8c91211ec").userId),
        Gen.const(fromString("cb9ef80a-22e9-434a-92bc-0bf060b6ef31").walletId),
        Gen.const(fromString("ac7c35f7-fb14-4df6-b006-2f18d50268a4").cardId)
      ),
      positiveAmountGen,
      numberGen,
      ccvGen,
      dateGen
    ) { case ((unknownCardId, userId, walletId, cardId), amount, number, ccv, expirationDate) =>
      val repository = newRepository()
      val service = new BankingService(repository)

      repository.createCard(EUR)(cardId, number, expirationDate, ccv)(userId)(walletId)

      service.loadCard(userId, unknownCardId.toString, amount).unsafeRunSync() match {
        case LoadCardCommandValidation.CardUnknown(_) => succeed
        case f => fail(s"Should be Card Unknown : $f")
      }
    }
  }

  it should "not load a blocked card" in {
    forAll(
      Gen.zip(
        Gen.const(fromString("c117d7e1-b746-4190-99cd-91d8c91211ec").userId),
        Gen.const(fromString("cb9ef80a-22e9-434a-92bc-0bf060b6ef31").walletId),
        Gen.const(fromString("ac7c35f7-fb14-4df6-b006-2f18d50268a4").cardId)
      ),
      positiveAmountGen,
      numberGen,
      ccvGen,
      dateGen
    ) { case ((userId, walletId, cardId), amount, number, ccv, expirationDate) =>
      val repository = newRepository()
      val service = new BankingService(repository)
      import service._

      repository.createCard(EUR)(cardId, number, expirationDate, ccv)(userId)(walletId)
      repository.blockCard(cardId)

      loadCard(userId, cardId.toString, amount).unsafeRunSync() match {
        case LoadCardCommandValidation.CardBlocked(_) => succeed
        case f => fail(s"Should be Card Blocked : $f")
      }
    }
  }

}
