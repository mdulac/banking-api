package model.commands

import cats.effect.IO
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
  implicit val entityDecoder: EntityDecoder[IO, CreateWalletCommand] = jsonOf[IO, CreateWalletCommand]
}