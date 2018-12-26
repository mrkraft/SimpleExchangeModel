package exchange.model

import exchange.model.entity.{Order, OrderInfo}
import exchange.util.DBUtils

/**
  * Основной класс для управления механизмами биржи.
  *
  * Ограничения:
  * - пользователь не может продавать себе
  * - если у поользователя недостаточно средств на балансе, заявка не создаётся(пропадает)
  */
class Exchange {

  val clientAccountManager = new ClientAccountManager(DBUtils.loadClientAccountData())
  val clientOrderProvider = new ClientOrderProvider(processNextOrder)
  val exchangeOrderPool = new ExchangeOrderPool()

  /**
    * Запуск работы биржи
    */
  def start() = {
    clientOrderProvider.sendOrders()

    //собираем данные после окончания обработки
    clientAccountManager.processAccountChanges(
      exchangeOrderPool.getUnprocessedOrders().map(_.getUnblockedAccountChange())
    )
    clientAccountManager.saveClientAccountData()
  }

  /**
    * Получает новые заявки и обрабатывает их
    */
  def processNextOrder(orderInfo: OrderInfo): Unit = {

    // если новая заявка погасилась неполностью, повторяем обработку для остаточной заявки
    def findAndMergeOrders(incomingOrder: Order): Unit = {
      exchangeOrderPool.findOrderForTrade(incomingOrder) match {
        case Some(exchangeOrder) => {
          val result = Order.mergeExchangeOrderWithIncomingOrder(exchangeOrder, incomingOrder)
          clientAccountManager.processAccountChanges(result.accountChanges)
          result.exchangeOrderPart.map(exchangeOrderPool.addOrder(_))
          result.incomingOrderPart.map(findAndMergeOrders(_))
        }
        case None => exchangeOrderPool.addOrder(incomingOrder)
      }
    }

    clientAccountManager.checkBalanceAndProcess(orderInfo).map {
      orderInfo =>
        val incomingOrder = Order(orderInfo)
        findAndMergeOrders(incomingOrder)
    }
  }

}

object Exchange {

  val STOCK_LIST = List('A', 'B', 'C', 'D')

  object Operation extends Enumeration {
    type Operation = Value
    val s, b = Value
  }
}
