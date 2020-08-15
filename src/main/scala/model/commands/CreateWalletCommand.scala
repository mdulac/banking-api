package model.commands

import cats.effect.Sync
import io.circe.generic.auto._
import model.Currency
import org.http4s.EntityDecoder
import org.http4s.circe._


final case class CreateWalletCommand(
                                      balance: BigDecimal,
                                      currency: Currency,
                                      isMaster: Boolean
                                    )

object CreateWalletCommand {
  implicit def entityDecoder[F[_] : Sync]: EntityDecoder[F, CreateWalletCommand] = jsonOf[F, CreateWalletCommand]
}