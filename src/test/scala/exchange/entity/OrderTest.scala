package exchange.entity

import exchange.model.Exchange.Operation
import exchange.model.entity.ClientAccount.{DollarAccountChanges, StockAccountChanges}
import exchange.model.entity.{Order, OrderInfo}
import org.scalatest._

class OrderTest extends FreeSpec with Matchers {

  trait Fixture {
    val orderSell1 = Order(OrderInfo("TEST", Operation.s, 'A', 10, 1))
    val orderBuy1 = Order(OrderInfo("TEST", Operation.b, 'A', 1, 1))

    val orderSell2 = Order(OrderInfo("TEST1", Operation.s, 'A', 20, 1))
    val orderBuy2 = Order(OrderInfo("TEST2", Operation.s, 'A', 1, 1))

    val orderSell3 = Order(OrderInfo("TEST1", Operation.s, 'A', 30, 1))
    val orderBuy3 = Order(OrderInfo("TEST2", Operation.b, 'B', 1, 1))


    val exchangeOrderSell1 = Order(OrderInfo("TEST1", Operation.s, 'A', 30, 1))
    val incomingOrderBuy1 = Order(OrderInfo("TEST2", Operation.b, 'A', 40, 2))

    val exchangeOrderBuy2 = Order(OrderInfo("TEST1", Operation.b, 'A', 12, 2))
    val incomingOrderSell2 = Order(OrderInfo("TEST2", Operation.s, 'A', 11, 1))

    val exchangeOrderBuy3 = Order(OrderInfo("TEST1", Operation.b, 'A', 15, 100))
    val incomingOrderSell3 = Order(OrderInfo("TEST2", Operation.s, 'A', 10, 100))
  }

  "Проверка простых условий несоответствия" in new Fixture {
    orderSell1.isMergeableWith(orderBuy1) shouldBe false
    orderBuy2.isMergeableWith(orderSell2) shouldBe false
    orderSell3.isMergeableWith(orderBuy3) shouldBe false
  }

  "Проверка условий соответствия по цене" in new Fixture {
    exchangeOrderSell1.isMergeableWith(incomingOrderBuy1) shouldBe true
    exchangeOrderBuy2.isMergeableWith(incomingOrderSell2) shouldBe true
  }


  "Проверка матчинга с остатком второй заявки" in new Fixture {
    val result = Order.mergeExchangeOrderWithIncomingOrder(exchangeOrderSell1, incomingOrderBuy1)
    result.exchangeOrderPart shouldBe None
    result.incomingOrderPart.get.info shouldBe OrderInfo("TEST2", Operation.b, 'A', 40, 1)
    result.accountChanges.head shouldBe DollarAccountChanges("TEST1", 30)
    result.accountChanges.last shouldBe StockAccountChanges("TEST2", 'A', 1)
  }

  "Проверка матчинга с остатком первой заявки" in new Fixture {
    val result = Order.mergeExchangeOrderWithIncomingOrder(exchangeOrderBuy2, incomingOrderSell2)
    result.exchangeOrderPart.get.info shouldBe OrderInfo("TEST1", Operation.b, 'A', 12, 1)
    result.incomingOrderPart shouldBe None
    result.accountChanges.head shouldBe DollarAccountChanges("TEST2", 12)
    result.accountChanges.last shouldBe StockAccountChanges("TEST1", 'A', 1)
  }

  "Проверка полного матчинга заявок" in new Fixture {
    val result = Order.mergeExchangeOrderWithIncomingOrder(exchangeOrderBuy3, incomingOrderSell3)
    result.exchangeOrderPart shouldBe None
    result.incomingOrderPart shouldBe None
    result.accountChanges.head shouldBe DollarAccountChanges("TEST2", 1500)
    result.accountChanges.last shouldBe StockAccountChanges("TEST1", 'A', 100)
  }
}
