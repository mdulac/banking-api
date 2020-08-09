package model.commands

import cats.effect.IO
import io.circe.generic.auto._
import model.Transfer
import model.Wallet.WalletId
import org.http4s.EntityDecoder
import org.http4s.circe.jsonOf

import model.Wallet.WalletId.decoder

final case class TransferCommand(
                                  amount: BigDecimal,
                                  source: WalletId,
                                  target: WalletId
                                )

object TransferCommand {
  implicit val entityDecoder: EntityDecoder[IO, TransferCommand] = jsonOf[IO, TransferCommand]
}

sealed trait TransferCommandValidation

object TransferCommandValidation {

  final case class WalletUnknown(walletId: WalletId) extends TransferCommandValidation

  final case class NotWalletOwner(walletId: WalletId) extends TransferCommandValidation

  final case class WalletBalanceTooLow(walletId: WalletId, balance: BigDecimal) extends TransferCommandValidation

  final case class Transfered(transfer: Transfer) extends TransferCommandValidation

  def walletUnknown(walletId: WalletId): TransferCommandValidation = WalletUnknown(walletId)

  def notWalletOwner(walletId: WalletId): TransferCommandValidation = NotWalletOwner(walletId)

  def walletBalanceTooLow(walletId: WalletId, balance: BigDecimal): TransferCommandValidation = WalletBalanceTooLow(walletId, balance)

  def transfered(transfer: Transfer): TransferCommandValidation = Transfered(transfer)
}