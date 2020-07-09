package simulation

import simulation.core.{Coins, Items, Money, ResultPair}

package object core {
  type Money = Int
  type Coins = Map[Coin, Int]
  type Items = Map[Item, Int]
  type ResultPair = (PurchaseResult, VendingMachine)
}

trait Coin {
  val value: Money
}

trait Item {
  val name: String
}

trait PurchaseValidationService {
  val priceService: PriceService

  def validateAndSale(item: Item, coins: List[Coin], fnSale: (Money, VendingMachine) => (PurchaseResult, VendingMachine))(implicit machine: VendingMachine): (PurchaseResult, VendingMachine) = {
    val amountIn = coins.map(_.value).sum
    if (!machine.items.contains(item))
      (FailedPurchase("Item not found"), machine)
    else priceService.getPrice(item) match {
      case Right(price) => if (price > amountIn)
        (FailedPurchase(s"Please add coins amount: ${price - amountIn}"), machine)
      else
        fnSale(price, machine)
      case Left(error) =>
        (FailedPurchase(error.getMessage), machine)
    }
  }

}

trait PriceService {
  def getPrice(item: Item): Either[Error, Money]
}

sealed trait PurchaseResult

final case class SuccessPurchase(coins: List[Coin]) extends PurchaseResult

final case class FailedPurchase(reason: String) extends PurchaseResult

final case class VendingMachine(coinBox: Coins, items: Items)

package object VendingMachineExtension {

  def changeReturn(amount: Int)(implicit denomination: List[Coin]): Iterable[Coins] = {
    def changeAcc(in: Money, acc: List[List[Coin]]): Iterable[List[Coin]] = in match {
        case 0 => acc
        case x if x > 0 => {
          val res = for {
            candidate <- denomination.map(c => (x - c.value, c))
            if candidate._1 >= 0
            xs = acc.map(ls => ls :+ candidate._2)
            t = changeAcc(candidate._1, xs)
          } yield t
          res.flatten
        }
      }

    val res = for {
      c <- denomination
      if amount - c.value >= 0
      xs = List(c)
    } yield changeAcc(amount - c.value, List(xs))
    val t = res.flatten
    t.map(l => l.foldLeft(Map[Coin, Int]())((acc, c) => if (acc.contains(c)) acc updated(c, acc(c)) else acc + (c -> 1)))
  }

  implicit class VendingMachineFunction(that: VendingMachine)(implicit purchaseService: PurchaseValidationService) {

    def addCoins(coins: List[Coin]): VendingMachine = {
      val newBox = coins.foldLeft(that.coinBox)((acc, coin) =>
        if (acc.contains(coin)) {
          val count = acc(coin)
          acc + (coin -> (count + 1))
        } else {
          acc + (coin -> 1)
        })
      VendingMachine(newBox, that.items)
    }

    // method can be used to add and remove items from VendingMachine
    def addItem(item: Item, number: Int): VendingMachine = {
      val newItems: Items = if (that.items.contains(item)) {
        val count = that.items(item)
        that.items updated(item, count + number)
      } else
        that.items + (item -> number)
      VendingMachine(that.coinBox, newItems)
    }

    def removeItem(item: Item): VendingMachine = if (that.items.contains(item) && that.items(item) > 0) {
      this.addItem(item, -1)
    } else throw new Error(s"Can not remove item: $item")

    def purchaseItem(item: Item, coins: List[Coin])(implicit denomination: List[Coin]): ResultPair = {

      def localSale(price: Money, v: VendingMachine): ResultPair = {
        price - coins.map(_.value).sum match {
          case 0 => (SuccessPurchase(List.empty), // exact amount of coins dropped into machine
            that
              .addCoins(coins)
              .removeItem(item)
          )
          case change: Int if change > 0 => // need to return some change amount
            val changeVariations = changeReturn(change)
            if (changeVariations.find(candidate => candidate.forall(p => that.coinBox.contains(p._1) && that.coinBox(p._1) >= p._2)).isDefined)
              (SuccessPurchase(List.empty), VendingMachine(v.coinBox, v.items))
            else
              (FailedPurchase("Unable to return change"), VendingMachine(v.coinBox, v.items))
          case _ =>
            (FailedPurchase("Unknown scenario: negative return change"), VendingMachine(v.coinBox, v.items))
        }

      }

      purchaseService.validateAndSale(item, coins, localSale)(that)
    }

  }

}

object Runner {

  final case class EuroCent(value: Money) extends Coin

  def main(args: Array[String]): Unit = {
    implicit val denomination: List[Coin] = List(EuroCent(1), EuroCent(2), EuroCent(5), EuroCent(10), EuroCent(20), EuroCent(50)).reverse
    val result = VendingMachineExtension.changeReturn(18)
    val data = result
    println(data mkString("\n"))
  }

}