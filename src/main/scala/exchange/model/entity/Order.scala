package exchange.model.entity

import exchange.model.Exchange.Operation
import exchange.model.entity.ClientAccount.{AccountChanges, DollarAccountChanges, StockAccountChanges}

/**
  * Класс заявки созданной на бирже (которая удовлетворяет условиям создания)
  * Т.к. заявки могуд приходить в одинаковое время, добавлен счётчик заявок insideId,
  * чтобы мы могли правильно сортировать заявки.
  */
case class Order(info: OrderInfo, createdAt: Long = System.currentTimeMillis(), insideId: Long = Order.getNextInsideId()) {

  /**
    * Проверка возможности матчинга с другой заявкой
    */
  def isMergeableWith(that: Order): Boolean = {
    this.info.isMergeableWith(that.info)
  }

  /**
    * Сравнивает по цене и дате создания
    */
  def compareTo (that: Order): Int = {

    // сравнение по цене
    if (this.info.stockPrice == that.info.stockPrice ) {

      // сравнение по дате создания
      if (this.createdAt == that.createdAt) {

        // сравнение по внутреннему индексу заявки
        if (this.insideId == that.insideId) {
            0
        } else if (this.insideId > that.insideId) {
          1
        } else {
          -1
        }
      } else if (this.createdAt > that.createdAt) {
        1
      } else {
        -1
      }
    } else if (this.info.stockPrice > that.info.stockPrice) {
      1
    } else {
      -1
    }
  }

  /**
    * Возвращает объект для разблокировки средств пользователя, которые были заблокированы для этой заявки
    */
  def getUnblockedAccountChange(): AccountChanges = {
    info.getUnblockedAccountChange()
  }
}

object Order {

  private var insideIdCounter = 0L
  def getNextInsideId(): Long = {
    insideIdCounter += 1
    insideIdCounter
  }

  case class MergeResult(exchangeOrderPart: Option[Order], incomingOrderPart: Option[Order], accountChanges: Iterable[AccountChanges])

  /**
    * Производит матчинг заявки из пула биржи и новой заявки.
    * Возвращает не полностью сматченые заявки и изменения балансов клиентов.
    */
  def mergeExchangeOrderWithIncomingOrder(exchangeOrder: Order, incomingOrder: Order): MergeResult = {
    val price = exchangeOrder.info.stockPrice
    val stockAmount = Math.min(exchangeOrder.info.stockAmount, incomingOrder.info.stockAmount)

    MergeResult(
      exchangeOrderPart =
        if (exchangeOrder.info.stockAmount > incomingOrder.info.stockAmount)
          Some(exchangeOrder.copy(info =  exchangeOrder.info.copy(stockAmount = exchangeOrder.info.stockAmount - incomingOrder.info.stockAmount)))
        else None,
      incomingOrderPart =
        if (incomingOrder.info.stockAmount > exchangeOrder.info.stockAmount)
          Some(incomingOrder.copy(info = incomingOrder.info.copy(stockAmount = incomingOrder.info.stockAmount - exchangeOrder.info.stockAmount)))
        else None,
      accountChanges = List(
        DollarAccountChanges(
          clientId = if(exchangeOrder.info.operation == Operation.s) exchangeOrder.info.clientId else incomingOrder.info.clientId,
          amount = price * stockAmount
        ),
        StockAccountChanges(
          clientId = if(exchangeOrder.info.operation == Operation.b) exchangeOrder.info.clientId else incomingOrder.info.clientId,
          stockId = exchangeOrder.info.stockId,
          amount = stockAmount
        )
      )
    )
  }

  object AscendingPriceTimeOrdering extends Ordering[Order] {
    def compare(order1:Order, order2:Order) = order1.compareTo(order2)
  }

  object DescendingPriceTimeOrdering extends Ordering[Order] {
    def compare(order1:Order, order2:Order) = order2.compareTo(order1)
  }
}
