package exchange.model

import exchange.model.Exchange.Operation
import exchange.model.entity.{Order, OrderInfo}
import org.scalatest.{FreeSpec, Matchers}

class ExchangeOrderPoolTest extends FreeSpec with Matchers {

  trait Fixture {
    val exchangeOrderSell1 = Order(OrderInfo("TEST1", Operation.s, 'A', 30, 1))
    val exchangeOrderSell2 = Order(OrderInfo("TEST1", Operation.s, 'A', 11, 1))
    val exchangeOrderSell3 = Order(OrderInfo("TEST1", Operation.s, 'A', 10, 100))

    val exchangeOrderBuy1 = Order(OrderInfo("TEST1", Operation.b, 'B', 40, 2))
    val exchangeOrderBuy2 = Order(OrderInfo("TEST1", Operation.b, 'B', 12, 2))
    val exchangeOrderBuy3 = Order(OrderInfo("TEST1", Operation.b, 'B', 15, 100))


    val exchangeOrderPool = new ExchangeOrderPool()

    exchangeOrderPool.addOrder(exchangeOrderSell1)
    exchangeOrderPool.addOrder(exchangeOrderSell3)
    exchangeOrderPool.addOrder(exchangeOrderSell2)

    exchangeOrderPool.addOrder(exchangeOrderBuy3)
    exchangeOrderPool.addOrder(exchangeOrderBuy2)
    exchangeOrderPool.addOrder(exchangeOrderBuy1)

    val incomingOrderSell = Order(OrderInfo("TEST2", Operation.s, 'B', 20, 100))

    val incomingOrderBuy1 = Order(OrderInfo("TEST2", Operation.b, 'A', 30, 2))
    val incomingOrderBuy2 = Order(OrderInfo("TEST2", Operation.b, 'A', 9, 100))
  }

  "Находит лучшую завку для матчинга" in new Fixture {
    exchangeOrderPool.findOrderForTrade(incomingOrderSell).get.info shouldBe OrderInfo("TEST1", Operation.b, 'B', 40, 2)
    exchangeOrderPool.findOrderForTrade(incomingOrderBuy1).get.info shouldBe OrderInfo("TEST1", Operation.s, 'A', 10, 100)
  }

  "Не находит подходящую заявку для матчинга" in new Fixture {
    exchangeOrderPool.findOrderForTrade(incomingOrderBuy2) shouldBe None
  }
}
