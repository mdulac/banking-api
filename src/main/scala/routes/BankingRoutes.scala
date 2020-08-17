package routes

import java.time.LocalDate.now
import java.util.UUID.randomUUID

import cats.data.ValidatedNel
import cats.effect.Sync
import cats.implicits.{catsStdInstancesForOption, catsSyntaxTuple2Semigroupal, catsSyntaxValidatedId}
import cats.syntax.flatMap.toFlatMapOps
import cats.{Applicative, FlatMap}
import io.circe.generic.auto._
import model.AuthenticationStatus.{Authenticated, NotAllowed}
import model.Card.CardId.{CardIdOps, encoder}
import model.Company.CompanyId.encoder
import model.Currency.currencyCodec
import model.Transfer.TransferId.encoder
import model.User.UserId.encoder
import model.Wallet.WalletId.encoder
import model.commands.CredentialsValidation.FieldMissing
import model.commands._
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Content-Type`
import org.http4s.util.CaseInsensitiveString
import org.http4s.{HttpRoutes, MediaType, Request}
import services.BankingService

import scala.util.Random

class BankingRoutes[F[_] : Sync : FlatMap, Query[_] : Sync : FlatMap](service: BankingService[F, Query]) extends Http4sDsl[F] {

  private def randomCcv = LazyList.iterate(Random.nextInt(10))(_ => Random.nextInt(10)).take(3).mkString("")

  private def randomNumber = LazyList.iterate(Random.nextInt(10))(_ => Random.nextInt(10)).take(16).mkString("")

  private def checkCredentials(request: Request[F]): ValidatedNel[CredentialsValidation, Credentials] = Applicative[Option].map2(
    request.headers.get(CaseInsensitiveString("User-Id")).map(_.value),
    request.headers.get(CaseInsensitiveString("Company-Id")).map(_.value)
  ) {
    case (userId, companyId) => (CredentialsValidation.validateUserId(userId), CredentialsValidation.validateCompanyId(companyId)).mapN(Credentials.apply)
  }.getOrElse(FieldMissing.invalidNel[Credentials])

  val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    case GET -> Root / "companies" =>
      service.listCompanies.flatMap(companies => Ok(companies, `Content-Type`(MediaType.application.json)))

    case GET -> Root / "users" =>
      service.listUsers.flatMap(users => Ok(users, `Content-Type`(MediaType.application.json)))

    case request@GET -> Root / "cards" =>
      val credentials = checkCredentials(request)
      credentials.fold(
        errors => BadRequest(errors),
        credentials =>
          service.authenticate(credentials).flatMap {
            case NotAllowed => Forbidden(s"${credentials.userId} is not part of ${credentials.companyId}")
            case Authenticated(userId, _) =>
              service.listCards(userId).flatMap(cards => Ok(cards, `Content-Type`(MediaType.application.json)))
          }
      )

    case request@GET -> Root / "wallets" =>
      val credentials = checkCredentials(request)
      credentials.fold(
        errors => BadRequest(errors),
        credentials =>
          service.authenticate(credentials).flatMap {
            case NotAllowed => Forbidden(s"${credentials.userId} is not part of ${credentials.companyId}")
            case Authenticated(_, companyId) =>
              service.listWallets(companyId).flatMap(wallets => Ok(wallets, `Content-Type`(MediaType.application.json)))
          }
      )

    case request@POST -> Root / "wallets" =>
      val credentials = checkCredentials(request)
      credentials.fold(
        errors => BadRequest(errors),
        credentials =>
          service.authenticate(credentials).flatMap {
            case NotAllowed => Forbidden(s"${credentials.userId} is not part of ${credentials.companyId}")
            case Authenticated(_, companyId) =>
              request.as[CreateWalletCommand].flatMap {
                command =>
                  service.createWallet(randomUUID())(companyId)(command)
                    .flatMap(wallet => Created(wallet, `Content-Type`(MediaType.application.json)))
              }
          }
      )

    case request@POST -> Root / "cards" / cardId / "load" =>
      val credentials = checkCredentials(request)
      credentials.fold(
        errors => BadRequest(errors),
        credentials =>
          service.authenticate(credentials).flatMap {
            case NotAllowed => Forbidden(s"${credentials.userId} is not part of ${credentials.companyId}")
            case Authenticated(userId, _) =>
              request.as[LoadCardCommand].flatMap {
                command =>
                  service.loadCard(userId, cardId, command.amount)
                    .flatMap {
                      case LoadCardCommandValidation.CardUnknown(cardId) => NotFound(s"Card $cardId is unknown")
                      case LoadCardCommandValidation.NotCardOwner(userId, cardId) => Forbidden(s"$userId is not card $cardId owner")
                      case LoadCardCommandValidation.CardBlocked(cardId) => BadRequest(s"Card $cardId is blocked")
                      case LoadCardCommandValidation.WalletBalanceTooLow(walletId, balance) => BadRequest(s"Wallet $walletId has a too low balance : $balance")
                      case LoadCardCommandValidation.CardCredited(cardId, balance) => Ok(s"Card $cardId is now $balance")
                    }
              }
          }
      )

    case request@POST -> Root / "cards" / cardId / "block" =>
      val credentials = checkCredentials(request)
      credentials.fold(
        errors => BadRequest(errors),
        credentials =>
          service.authenticate(credentials).flatMap {
            case NotAllowed => Forbidden(s"${credentials.userId} is not part of ${credentials.companyId}")
            case Authenticated(userId, _) =>
              service.blockCard(userId, cardId)
                .flatMap {
                  case BlockCardCommandValidation.CardUnknown(cardId) => NotFound(s"Card $cardId is unknown")
                  case BlockCardCommandValidation.NotCardOwner(userId, cardId) => Forbidden(s"$userId is not card $cardId owner")
                  case BlockCardCommandValidation.CardAlreadyBlocked(cardId) => BadRequest(s"Card $cardId is already blocked")
                  case BlockCardCommandValidation.CardBlocked(cardId) => Ok(s"Card $cardId is now blocked")
                }
          }
      )

    case request@POST -> Root / "cards" / cardId / "unblock" =>
      val credentials = checkCredentials(request)
      credentials.fold(
        errors => BadRequest(errors),
        credentials =>
          service.authenticate(credentials).flatMap {
            case NotAllowed => Forbidden(s"${credentials.userId} is not part of ${credentials.companyId}")
            case Authenticated(userId, _) =>
              service.unblockCard(userId, cardId)
                .flatMap {
                  case UnblockCardCommandValidation.CardUnknown(cardId) => NotFound(s"Card $cardId is unknown")
                  case UnblockCardCommandValidation.NotCardOwner(userId, cardId) => Forbidden(s"$userId is not card $cardId owner")
                  case UnblockCardCommandValidation.CardAlreadyUnblocked(cardId) => BadRequest(s"Card $cardId is already unblocked")
                  case UnblockCardCommandValidation.CardUnblocked(cardId) => Ok(s"Card $cardId is now unblocked")
                }
          }
      )

    case request@POST -> Root / "cards" =>
      val credentials = checkCredentials(request)
      credentials.fold(
        errors => BadRequest(errors),
        credentials =>
          service.authenticate(credentials).flatMap {
            case NotAllowed => Forbidden(s"${credentials.userId} is not part of ${credentials.companyId}")
            case Authenticated(userId, companyId) =>
              request.as[CreateCardCommand].flatMap {
                command =>
                  service.createCard(randomUUID().cardId, randomNumber, now().plusMonths(1), randomCcv, userId, companyId)(command)
                    .flatMap {
                      case CreateCardCommandValidation.NotWalletOwner(walletId) => Forbidden(s"$userId is not wallet $walletId owner")
                      case CreateCardCommandValidation.CardCreated(card) => Created(card, `Content-Type`(MediaType.application.json))
                    }
              }
          }
      )

    case request@POST -> Root / "transfer" =>
      val credentials = checkCredentials(request)
      credentials.fold(
        errors => BadRequest(errors),
        credentials =>
          service.authenticate(credentials).flatMap {
            case NotAllowed => Forbidden(s"${credentials.userId} is not part of ${credentials.companyId}")
            case Authenticated(userId, companyId) =>
              request.as[TransferCommand].flatMap {
                command =>
                  service.transfer(companyId)(command.amount, command.source, command.target)
                    .flatMap {
                      case TransferCommandValidation.WalletUnknown(walletId) => NotFound(s"Wallet $walletId is unknown")
                      case TransferCommandValidation.NotWalletOwner(walletId) => Forbidden(s"$userId is not wallet $walletId owner")
                      case TransferCommandValidation.WalletBalanceTooLow(walletId, balance) => BadRequest(s"Wallet $walletId has a too low balance : $balance")
                      case TransferCommandValidation.Transfered(transfer) => Ok(transfer, `Content-Type`(MediaType.application.json))
                    }
              }
          }
      )
  }

}
