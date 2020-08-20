package routes

import java.time.LocalDate.now
import java.util.UUID.randomUUID

import cats.data.OptionT.liftF
import cats.data.{Kleisli, ValidatedNel}
import cats.effect.Sync
import cats.implicits.{catsKernelStdMonoidForString, catsStdInstancesForOption, catsSyntaxEitherId, catsSyntaxTuple2Semigroupal, catsSyntaxValidatedId, _}
import cats.syntax.flatMap.toFlatMapOps
import cats.{Applicative, FlatMap}
import eu.timepit.refined.{refineMV, refineV}
import io.chrisdavenport.log4cats.Logger
import io.circe.generic.auto._
import model.AuthenticationStatus.{Authenticated, NotAllowed}
import model.Card
import model.Card.CardId.CardIdOps
import model.Currency.currencyCodec
import model.Wallet.WalletId.encoder
import model.commands.CredentialsValidation.FieldMissing
import model.commands._
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Content-Type`
import org.http4s.server.AuthMiddleware
import org.http4s.util.CaseInsensitiveString
import org.http4s.{Credentials => _, _}
import services.BankingService
import io.circe.refined._

import scala.util.Random

class BankingRoutes[F[_] : Sync : FlatMap : Logger, Q[_]](service: BankingService[F, Q]) extends Http4sDsl[F] {

  private def randomCcv = refineV[Card.Ccv](LazyList.iterate(Random.nextInt(10))(_ => Random.nextInt(10)).take(3).mkString(""))

  private def randomNumber = refineV[Card.Number](LazyList.iterate(Random.nextInt(10))(_ => Random.nextInt(10)).take(16).mkString(""))

  private def checkCredentials(request: Request[F]): ValidatedNel[CredentialsValidation, Credentials] = Applicative[Option].map2(
    request.headers.get(CaseInsensitiveString("User-Id")).map(_.value),
    request.headers.get(CaseInsensitiveString("Company-Id")).map(_.value)
  ) {
    case (userId, companyId) => (CredentialsValidation.validateUserId(userId), CredentialsValidation.validateCompanyId(companyId)).mapN(Credentials.apply)
  }.getOrElse(FieldMissing.invalidNel[Credentials])

  val authenticate: Kleisli[F, Request[F], Either[String, Credentials]] = Kleisli { request =>
    checkCredentials(request).fold(
      errors => Sync[F].point(errors.map(_.message).reduce.asLeft[Credentials]),
      c => service.authenticate(c).flatMap {
        case NotAllowed(userId, companyId) =>
          Logger[F].info(s"$userId is not part of $companyId") *> Sync[F].point(s"$userId is not part of $companyId".asLeft[Credentials])
        case Authenticated(userId, _) =>
          Logger[F].info(s"User $userId authenticated") *> Sync[F].point(c.asRight[String])
      }
    )
  }

  val onFailure: AuthedRoutes[String, F] = Kleisli(req => liftF(Forbidden(req.context)))
  val middleware: AuthMiddleware[F, Credentials] = AuthMiddleware(authenticate, onFailure)

  val nonAuthedRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "companies" => service.listCompanies.flatMap(companies => Ok(companies, `Content-Type`(MediaType.application.json)))
    case GET -> Root / "users" => service.listUsers.flatMap(users => Ok(users, `Content-Type`(MediaType.application.json)))
  }

  val authedRoutes: AuthedRoutes[Credentials, F] = AuthedRoutes.of[Credentials, F] {
    case GET -> Root / "cards" as credentials =>
      service.listCards(credentials.userId).flatMap(cards => Ok(cards, `Content-Type`(MediaType.application.json)))

    case GET -> Root / "wallets" as credentials =>
      service.listWallets(credentials.companyId).flatMap(wallets => Ok(wallets, `Content-Type`(MediaType.application.json)))

    case request@POST -> Root / "wallets" as credentials =>
      request.req.as[CreateWalletCommand].flatMap {
        command =>
          service.createWallet(randomUUID())(credentials.companyId)(command)
            .flatMap { wallet =>
              Logger[F].info(s"Wallet created") *> Created(wallet, `Content-Type`(MediaType.application.json))
            }
      }

    case request@POST -> Root / "cards" / cardId / "load" as credentials =>
      request.req.as[LoadCardCommand].flatMap {
        command =>
          service.loadCard(credentials.userId, cardId, command.amount)
            .flatMap {
              case LoadCardCommandValidation.CardUnknown(cardId) =>
                Logger[F].info(s"Card $cardId unknown") *> NotFound(s"Card $cardId unknown")
              case LoadCardCommandValidation.NotCardOwner(userId, cardId) =>
                Logger[F].info(s"$userId is not card $cardId owner") *> Forbidden(s"$userId is not card $cardId owner")
              case LoadCardCommandValidation.CardBlocked(cardId) =>
                Logger[F].info(s"Card $cardId is blocked") *> BadRequest(s"Card $cardId is blocked")
              case LoadCardCommandValidation.WalletBalanceTooLow(walletId, balance) =>
                Logger[F].info(s"Wallet $walletId has a too low balance : $balance") *> BadRequest(s"Wallet $walletId has a too low balance : $balance")
              case LoadCardCommandValidation.CardCredited(cardId, balance) =>
                Logger[F].info(s"Card $cardId is now $balance") *> Ok(s"Card $cardId is now $balance")
            }
      }

    case POST -> Root / "cards" / cardId / "block" as credentials =>
      service.blockCard(credentials.userId, cardId)
        .flatMap {
          case BlockCardCommandValidation.CardUnknown(cardId) =>
            Logger[F].info(s"Card $cardId unknown") *> NotFound(s"Card $cardId unknown")
          case BlockCardCommandValidation.NotCardOwner(userId, cardId) =>
            Logger[F].info(s"$userId is not card $cardId owner") *> Forbidden(s"$userId is not card $cardId owner")
          case BlockCardCommandValidation.CardAlreadyBlocked(cardId) =>
            Logger[F].info(s"Card $cardId is already blocked") *> BadRequest(s"Card $cardId is already blocked")
          case BlockCardCommandValidation.CardBlocked(cardId) =>
            Logger[F].info(s"Card $cardId is now blocked") *> Ok(s"Card $cardId is now blocked")
        }

    case POST -> Root / "cards" / cardId / "unblock" as credentials =>
      service.unblockCard(credentials.userId, cardId)
        .flatMap {
          case UnblockCardCommandValidation.CardUnknown(cardId) =>
            Logger[F].info(s"Card $cardId unknown") *> NotFound(s"Card $cardId unknown")
          case UnblockCardCommandValidation.NotCardOwner(userId, cardId) =>
            Logger[F].info(s"$userId is not card $cardId owner") *> Forbidden(s"$userId is not card $cardId owner")
          case UnblockCardCommandValidation.CardAlreadyUnblocked(cardId) =>
            Logger[F].info(s"Card $cardId is already unblocked") *> BadRequest(s"Card $cardId is already unblocked")
          case UnblockCardCommandValidation.CardUnblocked(cardId) =>
            Logger[F].info(s"Card $cardId is now unblocked") *> Ok(s"Card $cardId is now unblocked")
        }

    case request@POST -> Root / "cards" as credentials =>
      request.req.as[CreateCardCommand].flatMap {
        command =>
          service.createCard(randomUUID().cardId, randomNumber.fold(_ => refineMV[Card.Number]("0000000000000000"), identity), now().plusMonths(1), randomCcv.fold(_ => refineMV[Card.Ccv]("000"), identity), credentials.userId, credentials.companyId)(command)
            .flatMap {
              case CreateCardCommandValidation.NotWalletOwner(walletId) =>
                Logger[F].info(s"${credentials.userId} is not wallet $walletId owner") *> Forbidden(s"${credentials.userId} is not wallet $walletId owner")
              case CreateCardCommandValidation.CardCreated(card) =>
                Logger[F].info(s"Card created") *> Created(card, `Content-Type`(MediaType.application.json))
            }
      }

    case request@POST -> Root / "transfer" as credentials =>
      request.req.as[TransferCommand].flatMap {
        command =>
          service.transfer(credentials.companyId)(command.amount, command.source, command.target)
            .flatMap {
              case TransferCommandValidation.WalletUnknown(walletId) =>
                Logger[F].info(s"Wallet $walletId unknown") *> NotFound(s"Wallet $walletId unknown")
              case TransferCommandValidation.NotWalletOwner(walletId) =>
                Logger[F].info(s"${credentials.userId} is not wallet $walletId owner") *> Forbidden(s"${credentials.userId} is not wallet $walletId owner")
              case TransferCommandValidation.WalletBalanceTooLow(walletId, balance) =>
                Logger[F].info(s"Wallet $walletId has a too low balance : $balance") *> BadRequest(s"Wallet $walletId has a too low balance : $balance")
              case TransferCommandValidation.Transfered(transfer) =>
                Logger[F].info(s"Transfer processed") *> Ok(transfer, `Content-Type`(MediaType.application.json))
            }
      }
  }

  val routes: HttpRoutes[F] = nonAuthedRoutes <+> middleware(authedRoutes)

}
