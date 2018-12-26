package exchange.model

import exchange.model.Exchange.Operation
import exchange.model.entity.{ClientAccount, OrderInfo}
import exchange.model.entity.ClientAccount.{DollarAccountChanges, Stock, StockAccountChanges}
import org.scalatest._

class ClientAccountManagerTest extends FreeSpec with Matchers {

  trait Fixture {
    val stockA = Stock('A', 0)
    val stockB = Stock('B', 1)
    val stockC = Stock('C', 2)
    val stockD = Stock('D', 3)

    val stockMap = Map(
      stockA.id -> stockA,
      stockB.id -> stockB,
      stockC.id -> stockC,
      stockD.id -> stockD
    )

    val clientAccount = new ClientAccount("TEST", 1000, stockMap)
    val clientAccounts = Map(clientAccount.id -> clientAccount)

    val clientAccountManager = new ClientAccountManager(clientAccounts)

    val orderInfoSellGood = OrderInfo("TEST", Operation.s, 'B', 1, 1)
    val orderInfoSellBad = OrderInfo("TEST", Operation.s, 'A', 1, 10)
    val orderInfoBuyGood = OrderInfo("TEST", Operation.b, 'D', 15, 20)
    val orderInfoBuyBad = OrderInfo("TEST", Operation.b, 'C', 42, 100500)

    val dollarAccountChange = DollarAccountChanges("TEST", 99)
    val stockAccountChange = StockAccountChanges("TEST", 'A', 5000)
    val changes = List(dollarAccountChange, stockAccountChange)
  }

  "Если пришла подходящая заявка на продажу, возвращаем её" in new Fixture {
    clientAccountManager.checkBalanceAndProcess(orderInfoSellGood) shouldBe Some(orderInfoSellGood)
    clientAccountManager.getClientAccountsList().head.stockBalances.get('B') shouldBe Some(Stock('B', 0))
  }

  "Если пришла не соответствующая балансу заявка на продажу, возвращаем Null" in new Fixture {
    clientAccountManager.checkBalanceAndProcess(orderInfoSellBad) shouldBe None
    clientAccountManager.getClientAccountsList().head.stockBalances.get('A') shouldBe Some(Stock('A', 0))
  }

  "Если пришла подходящая заявка на покупку, возвращаем её" in new Fixture {
    clientAccountManager.checkBalanceAndProcess(orderInfoBuyGood) shouldBe Some(orderInfoBuyGood)
    clientAccountManager.getClientAccountsList().head.dollarAmount shouldBe 700
  }

  "Если пришла не соответствующая балансу заявка на покупку, возвращаем Null" in new Fixture {
    clientAccountManager.checkBalanceAndProcess(orderInfoBuyBad) shouldBe None
    clientAccountManager.getClientAccountsList().head.dollarAmount shouldBe 1000
  }

  "Балансы должны увеличиться" in new Fixture {
    clientAccountManager.processAccountChanges(changes)
    clientAccountManager.getClientAccountsList().head.dollarAmount shouldBe 1099
    clientAccountManager.getClientAccountsList().head.stockBalances.get('A') shouldBe Some(Stock('A', 5000))
  }
}
