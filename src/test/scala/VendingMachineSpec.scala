import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop._
import simulation.core.Money
import simulation._

import scala.util.Random

class VendingMachineSpec extends Properties("VendingMachine") {

  import VendingMachineExtension._

  val emptyVendingMachine = VendingMachine(Map.empty, Map.empty)

  case class TestCoin(value: Money) extends Coin

  case class TestItem(name: String) extends Item

  val coinsNominations = List(TestCoin(1), TestCoin(2), TestCoin(5), TestCoin(10), TestCoin(20), TestCoin(50))
  val itemTypes = List(TestItem("Coke"), TestItem("Pepsi"), TestItem("Fanta"))
  val prices : Map[Item, Money]= Map( TestItem("Coke") -> 30, TestItem("Pepsi") -> 25, TestItem("Fanta") -> 20)

  val coinGen = Gen.oneOf(coinsNominations)
  val coinListGen = Gen.containerOf[List, Coin](coinGen)

  val itemGen = Gen.oneOf(itemTypes)
  val itemsGen = Gen.containerOf[List, Item](itemGen)

  class TestPriceService extends PriceService {
    override def getPrice(item: Item): Either[Error,Money] =
      if (prices.contains(item))
        Right(prices(item))
      else
        Left(new Error(s"Price not found for: $item"))
  }

  object TestPriceSerive extends PriceService {
    override def getPrice(item: Item): Either[Error, Money] =
      if (prices.contains(item))
        Right( prices(item))
      else
        Left( new Error("No price found"))
  }

  implicit val purchaseService : PurchaseValidationService = new PurchaseValidationService {
    override val priceService: PriceService = TestPriceSerive
  }

  property("CoinBox") = forAll(coinListGen) { coins =>
    val vendingMachine = emptyVendingMachine.addCoins(coins)
    vendingMachine.coinBox.keys.forall(coin => coins.find(c => c.value == coin.value).isDefined)
  }

  property("Items") = forAll(itemsGen) { items =>
    val vendingMachine = items.foldLeft(emptyVendingMachine)((acc, i) => acc.addItem(i, Random.nextInt(10) + 1))
    vendingMachine.items.keys.forall(i => items.find(_.name == i.name).isDefined)
    vendingMachine.items.values.forall(count => count > 0)
  }

}