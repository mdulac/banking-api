package services

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID
import java.util.UUID.fromString

import cats.effect.{Sync, SyncIO}
import cats.implicits.catsSyntaxOptionId
import cats.{Id, Monad, ~>}
import eu.timepit.refined
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
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

  implicit def unsafeLogger[F[_]: Sync] = Slf4jLogger.getLogger[F]

  def newRepository(): BankingRepository[Id, SyncIO] = new BankingRepository[Id, SyncIO]() {

    private def modify[A](list: List[A])(f: A => Boolean)(m: A => A): List[A] = {
      list.find(f) match {
        case None => list
        case Some(el) => m(el) +: list.filter(a => !f(a))
      }
    }

    override implicit val Transform: Id ~> SyncIO = new (Id ~> SyncIO) {
      override def apply[A](fa: Id[A]): SyncIO[A] = SyncIO.apply(fa)
    }

    override val Instance: Monad[Id] = implicitly[Monad[Id]]

    override def authenticate(credentials: Credentials): Id[Option[(UserId, CompanyId)]] =
      (fromString("c117d7e1-b746-4190-99cd-91d8c91211ec").userId, fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId).some

    val companies = List(
      Company(fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId, "Google")
    )

    val users = List(
      User(fromString("c117d7e1-b746-4190-99cd-91d8c91211ec").userId, fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId),
      User(fromString("b8a235a0-f39d-4d06-a8b1-79ec9510df29").userId, fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId)
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

  private val numberGen: Gen[String] = Gen.listOfN(16, Gen.choose(0, 1)).map(_.mkString(""))
  private val ccvGen: Gen[String] = Gen.listOfN(3, Gen.choose(0, 1)).map(_.mkString(""))
  private val amountGen: Gen[BigDecimal] = Gen.choose(1, 1000).map(BigDecimal.apply)
  private val positiveAmountGen: Gen[BigDecimal Refined Positive] =
    amountGen
      .map(refined.refineV[Positive].apply[BigDecimal])
      .flatMap {
        case Right(value) => Gen.const(value)
        case Left(_) => Gen.fail
      }
  private val currencyGen: Gen[Currency] = Gen.oneOf(EUR, GBP, USD)
  private val booleanGen: Gen[Boolean] = Gen.oneOf(true, false)
  private val dateGen: Gen[LocalDate] = Gen.const(LocalDate.now())

  behavior of "wallet creation"

  "A user" should "create a wallet" in {
    forAll(uuid, positiveAmountGen, currencyGen, booleanGen) { (walletId, balance, currency, isMaster) =>
      val companyId = fromString("84b75488-4c65-4cdf-be52-9a41e9c58c17").companyId

      val repository = newRepository()
      val service = new BankingService(repository)

      service.createWallet(walletId)(companyId)(CreateWalletCommand(balance, currency, isMaster)).unsafeRunSync() match {
        case Wallet(i, b, c, ci, m) =>
          i should be(WalletId(walletId))
          b should be(balance.value)
          c should be(currency)
          ci should be(companyId)
          m should be(isMaster)
      }
    }
  }

  behavior of "card creation"

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

  behavior of "card load"

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

  it should "not load a card if he's not the owner" in {
    forAll(
      Gen.zip(
        Gen.const(fromString("c117d7e1-b746-4190-99cd-91d8c91211ec").userId),
        Gen.const(fromString("b8a235a0-f39d-4d06-a8b1-79ec9510df29").userId),
        Gen.const(fromString("cb9ef80a-22e9-434a-92bc-0bf060b6ef31").walletId),
        Gen.const(fromString("ac7c35f7-fb14-4df6-b006-2f18d50268a4").cardId),
        Gen.const(fromString("5db6b5c0-e788-41c9-a6d0-e239af0b7f74").cardId)
      ),
      positiveAmountGen,
      numberGen,
      ccvGen,
      dateGen
    ) { case ((user1Id, user2Id, walletId, card1Id, card2Id), amount, number, ccv, expirationDate) =>
      val repository = newRepository()
      val service = new BankingService(repository)

      repository.createCard(EUR)(card1Id, number, expirationDate, ccv)(user1Id)(walletId)
      repository.createCard(EUR)(card2Id, number, expirationDate, ccv)(user2Id)(walletId)

      service.loadCard(user1Id, card2Id.toString, amount).unsafeRunSync() match {
        case LoadCardCommandValidation.NotCardOwner(userId, cardId) =>
          userId should be(user1Id)
          cardId should be(card2Id)
        case f => fail(s"Should be Not Card Owner : $f")
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

  behavior of "card block / unblock"

  behavior of "transfer"

}
