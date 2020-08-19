package model.commands

import cats.effect.Sync
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import io.circe.generic.auto._
import model.Currency
import org.http4s.EntityDecoder
import org.http4s.circe._
import io.circe.refined._


final case class CreateWalletCommand(
                                      balance: BigDecimal Refined Positive,
                                      currency: Currency,
                                      isMaster: Boolean
                                    )

object CreateWalletCommand {
  implicit def entityDecoder[F[_] : Sync]: EntityDecoder[F, CreateWalletCommand] = jsonOf[F, CreateWalletCommand]
}