package model

import doobie.Meta
import io.circe.Codec
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveEnumerationCodec

sealed trait Currency

object Currency {

  implicit val currencyMeta: Meta[Currency] = Meta[String].timap[Currency] {
    case "USD" => USD
    case "GBP" => GBP
    case "EUR" => EUR
  } {
    case USD => "USD"
    case GBP => "GBP"
    case EUR => "EUR"
  }

  private implicit val config: Configuration =
    Configuration.default.copy(transformConstructorNames = _.toUpperCase)

  implicit val currencyCodec: Codec[Currency] = deriveEnumerationCodec[Currency]

  final case object USD extends Currency

  final case object GBP extends Currency

  final case object EUR extends Currency

}
