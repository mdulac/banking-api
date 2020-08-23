package routes

import java.time.LocalDate.now
import java.util.UUID.randomUUID

import cats.Applicative
import cats.data.OptionT.liftF
import cats.data.{Kleisli, ValidatedNel}
import cats.effect.Sync
import cats.implicits.{catsKernelStdMonoidForString, catsStdInstancesForOption, catsSyntaxEitherId, catsSyntaxTuple2Semigroupal, catsSyntaxValidatedId, _}
import cats.syntax.flatMap.toFlatMapOps
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.circe.generic.auto._
import model.AuthenticationStatus.{Authenticated, NotAllowed}
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

import scala.util.Random

object BankingRoutes {
  def build[F[_] : Sync, Q[_]](service: BankingService[F, Q]) =
    Slf4jLogger.create[F].map(l => new BankingRoutes[F, Q](service, l))
}

class BankingRoutes[F[_] : Sync, Q[_]](service: BankingService[F, Q], logger: Logger[F]) extends Http4sDsl[F] {

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
          logger.info(show"$userId is not part of $companyId") *> Sync[F].point(show"$userId is not part of $companyId".asLeft[Credentials])
        case Authenticated(userId, _) =>
          logger.info(show"User $userId authenticated") *> Sync[F].point(c.asRight[String])
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
              logger.info(show"Wallet created") *> Created(wallet, `Content-Type`(MediaType.application.json))
            }
      }

    case request@POST -> Root / "cards" / UUIDVar(id) / "load" as credentials =>
      request.req.as[LoadCardCommand].flatMap {
        command =>
          service.loadCard(credentials.userId, id.cardId, command.amount)
            .flatMap {
              case LoadCardCommandValidation.CardUnknown(cardId) =>
                logger.info(show"Card $cardId unknown") *> NotFound(show"Card $cardId unknown")
              case LoadCardCommandValidation.NotCardOwner(userId, cardId) =>
                logger.info(show"$userId is not card $cardId owner") *> Forbidden(show"$userId is not card $cardId owner")
              case LoadCardCommandValidation.CardBlocked(cardId) =>
                logger.info(show"Card $cardId is blocked") *> BadRequest(show"Card $cardId is blocked")
              case LoadCardCommandValidation.WalletBalanceTooLow(walletId, balance) =>
                logger.info(show"Wallet $walletId has a too low balance : $balance") *> BadRequest(show"Wallet $walletId has a too low balance : $balance")
              case LoadCardCommandValidation.CardCredited(cardId, balance) =>
                logger.info(show"Card $cardId is now $balance") *> Ok(show"Card $cardId is now $balance")
            }
      }

    case POST -> Root / "cards" / UUIDVar(id) / "block" as credentials =>
      service.blockCard(credentials.userId, id.cardId)
        .flatMap {
          case BlockCardCommandValidation.CardUnknown(cardId) =>
            logger.info(show"Card $cardId unknown") *> NotFound(show"Card $cardId unknown")
          case BlockCardCommandValidation.NotCardOwner(userId, cardId) =>
            logger.info(show"$userId is not card $cardId owner") *> Forbidden(show"$userId is not card $cardId owner")
          case BlockCardCommandValidation.CardAlreadyBlocked(cardId) =>
            logger.info(show"Card $cardId is already blocked") *> BadRequest(show"Card $cardId is already blocked")
          case BlockCardCommandValidation.CardBlocked(cardId) =>
            logger.info(show"Card $cardId is now blocked") *> Ok(show"Card $cardId is now blocked")
        }

    case POST -> Root / "cards" / UUIDVar(id) / "unblock" as credentials =>
      service.unblockCard(credentials.userId, id.cardId)
        .flatMap {
          case UnblockCardCommandValidation.CardUnknown(cardId) =>
            logger.info(show"Card $cardId unknown") *> NotFound(show"Card $cardId unknown")
          case UnblockCardCommandValidation.NotCardOwner(userId, cardId) =>
            logger.info(show"$userId is not card $cardId owner") *> Forbidden(show"$userId is not card $cardId owner")
          case UnblockCardCommandValidation.CardAlreadyUnblocked(cardId) =>
            logger.info(show"Card $cardId is already unblocked") *> BadRequest(show"Card $cardId is already unblocked")
          case UnblockCardCommandValidation.CardUnblocked(cardId) =>
            logger.info(show"Card $cardId is now unblocked") *> Ok(show"Card $cardId is now unblocked")
        }

    case request@POST -> Root / "cards" as credentials =>
      request.req.as[CreateCardCommand].flatMap {
        command =>
          service.createCard(randomUUID().cardId, randomNumber, now().plusMonths(1), randomCcv, credentials.userId, credentials.companyId)(command)
            .flatMap {
              case CreateCardCommandValidation.NotWalletOwner(walletId) =>
                logger.info(show"${credentials.userId} is not wallet $walletId owner") *> Forbidden(show"${credentials.userId} is not wallet $walletId owner")
              case CreateCardCommandValidation.CardCreated(card) =>
                logger.info(show"Card created") *> Created(card, `Content-Type`(MediaType.application.json))
            }
      }

    case request@POST -> Root / "transfer" as credentials =>
      request.req.as[TransferCommand].flatMap {
        command =>
          service.transfer(credentials.companyId)(command.amount, command.source, command.target)
            .flatMap {
              case TransferCommandValidation.WalletUnknown(walletId) =>
                logger.info(show"Wallet $walletId unknown") *> NotFound(show"Wallet $walletId unknown")
              case TransferCommandValidation.NotWalletOwner(walletId) =>
                logger.info(show"${credentials.userId} is not wallet $walletId owner") *> Forbidden(show"${credentials.userId} is not wallet $walletId owner")
              case TransferCommandValidation.WalletBalanceTooLow(walletId, balance) =>
                logger.info(show"Wallet $walletId has a too low balance : $balance") *> BadRequest(show"Wallet $walletId has a too low balance : $balance")
              case TransferCommandValidation.Transfered(transfer) =>
                logger.info(show"Transfer processed") *> Ok(transfer, `Content-Type`(MediaType.application.json))
            }
      }
  }

  val routes: HttpRoutes[F] = nonAuthedRoutes <+> middleware(authedRoutes)

}
