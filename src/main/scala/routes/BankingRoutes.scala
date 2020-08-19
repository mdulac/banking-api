package routes

import java.time.LocalDate.now
import java.util.UUID.randomUUID

import cats.data.OptionT.liftF
import cats.data.{Kleisli, ValidatedNel}
import cats.effect.Sync
import cats.implicits.{catsKernelStdMonoidForString, catsStdInstancesForOption, catsSyntaxEitherId, catsSyntaxTuple2Semigroupal, catsSyntaxValidatedId, _}
import cats.syntax.flatMap.toFlatMapOps
import cats.{Applicative, FlatMap}
import io.chrisdavenport.log4cats.Logger
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
import org.http4s.server.AuthMiddleware
import org.http4s.util.CaseInsensitiveString
import org.http4s.{Credentials => _, _}
import services.BankingService

import scala.util.Random

class BankingRoutes[F[_] : Sync : FlatMap : Logger, Query[_]](service: BankingService[F, Query]) extends Http4sDsl[F] {

  private def randomCcv = LazyList.iterate(Random.nextInt(10))(_ => Random.nextInt(10)).take(3).mkString("")

  private def randomNumber = LazyList.iterate(Random.nextInt(10))(_ => Random.nextInt(10)).take(16).mkString("")

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
          Sync[F].point(s"$userId is not part of $companyId".asLeft[Credentials])
        case Authenticated(_, _) =>
          Sync[F].point(c.asRight[String])
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
      Logger[F].info("Create wallet route") *>
        request.req.as[CreateWalletCommand].flatMap {
          command =>
            service.createWallet(randomUUID())(credentials.companyId)(command)
              .flatMap(wallet => Created(wallet, `Content-Type`(MediaType.application.json)))
        }

    case request@POST -> Root / "cards" / cardId / "load" as credentials =>
      request.req.as[LoadCardCommand].flatMap {
        command =>
          service.loadCard(credentials.userId, cardId, command.amount)
            .flatMap {
              case LoadCardCommandValidation.CardUnknown(cardId) => NotFound(s"Card $cardId is unknown")
              case LoadCardCommandValidation.NotCardOwner(userId, cardId) => Forbidden(s"$userId is not card $cardId owner")
              case LoadCardCommandValidation.CardBlocked(cardId) => BadRequest(s"Card $cardId is blocked")
              case LoadCardCommandValidation.WalletBalanceTooLow(walletId, balance) => BadRequest(s"Wallet $walletId has a too low balance : $balance")
              case LoadCardCommandValidation.CardCredited(cardId, balance) => Ok(s"Card $cardId is now $balance")
            }
      }

    case POST -> Root / "cards" / cardId / "block" as credentials =>
      service.blockCard(credentials.userId, cardId)
        .flatMap {
          case BlockCardCommandValidation.CardUnknown(cardId) => NotFound(s"Card $cardId is unknown")
          case BlockCardCommandValidation.NotCardOwner(userId, cardId) => Forbidden(s"$userId is not card $cardId owner")
          case BlockCardCommandValidation.CardAlreadyBlocked(cardId) => BadRequest(s"Card $cardId is already blocked")
          case BlockCardCommandValidation.CardBlocked(cardId) => Ok(s"Card $cardId is now blocked")
        }

    case POST -> Root / "cards" / cardId / "unblock" as credentials =>
      service.unblockCard(credentials.userId, cardId)
        .flatMap {
          case UnblockCardCommandValidation.CardUnknown(cardId) => NotFound(s"Card $cardId is unknown")
          case UnblockCardCommandValidation.NotCardOwner(userId, cardId) => Forbidden(s"$userId is not card $cardId owner")
          case UnblockCardCommandValidation.CardAlreadyUnblocked(cardId) => BadRequest(s"Card $cardId is already unblocked")
          case UnblockCardCommandValidation.CardUnblocked(cardId) => Ok(s"Card $cardId is now unblocked")
        }

    case request@POST -> Root / "cards" as credentials =>
      request.req.as[CreateCardCommand].flatMap {
        command =>
          service.createCard(randomUUID().cardId, randomNumber, now().plusMonths(1), randomCcv, credentials.userId, credentials.companyId)(command)
            .flatMap {
              case CreateCardCommandValidation.NotWalletOwner(walletId) => Forbidden(s"${credentials.userId} is not wallet $walletId owner")
              case CreateCardCommandValidation.CardCreated(card) => Created(card, `Content-Type`(MediaType.application.json))
            }
      }

    case request@POST -> Root / "transfer" as credentials =>
      request.req.as[TransferCommand].flatMap {
        command =>
          service.transfer(credentials.companyId)(command.amount, command.source, command.target)
            .flatMap {
              case TransferCommandValidation.WalletUnknown(walletId) => NotFound(s"Wallet $walletId is unknown")
              case TransferCommandValidation.NotWalletOwner(walletId) => Forbidden(s"${credentials.userId} is not wallet $walletId owner")
              case TransferCommandValidation.WalletBalanceTooLow(walletId, balance) => BadRequest(s"Wallet $walletId has a too low balance : $balance")
              case TransferCommandValidation.Transfered(transfer) => Ok(transfer, `Content-Type`(MediaType.application.json))
            }
      }
  }

  val routes: HttpRoutes[F] = nonAuthedRoutes <+> middleware(authedRoutes)

}
