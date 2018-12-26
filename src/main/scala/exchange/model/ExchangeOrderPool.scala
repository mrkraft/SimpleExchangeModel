package exchange.model

import exchange.model.Exchange._
import exchange.model.entity.Order
import exchange.model.entity.Order.{AscendingPriceTimeOrdering, DescendingPriceTimeOrdering}

import scala.collection.mutable

/**
  * Класс для управления списком заявок, которые находятся в ожидании на бирже
  */
class ExchangeOrderPool {

  private val buyOrdersByStockMap: Map[Char, mutable.SortedSet[Order]] =
    STOCK_LIST.map(stockId => stockId -> new mutable.TreeSet[Order]()(DescendingPriceTimeOrdering)).toMap

  private val sellOrdersByStockMap: Map[Char, mutable.SortedSet[Order]] =
    STOCK_LIST.map(stockId => stockId -> new mutable.TreeSet[Order]()(AscendingPriceTimeOrdering)).toMap

  /**
    * Находит подходящую для матчинга с incomingOrder заявку
    */
  def findOrderForTrade(incomingOrder: Order): Option[Order] = {
    val ordersByStockMap = incomingOrder.info.operation match {
      case Operation.s => buyOrdersByStockMap
      case Operation.b => sellOrdersByStockMap
    }
    ordersByStockMap
      .get(incomingOrder.info.stockId)
      .flatMap(
        orderList =>
          orderList
            .find(_.isMergeableWith(incomingOrder))
            .map {
              exchangeOrder =>
                orderList.remove(exchangeOrder)
                exchangeOrder
            }
      )
  }

  /**
    * Добавляет заявку в пул
    */
  def addOrder(order: Order): Unit = {
    val ordersByStockMap = order.info.operation match {
      case Operation.s => sellOrdersByStockMap
      case Operation.b => buyOrdersByStockMap
    }
    ordersByStockMap.get(order.info.stockId).map(orderList => orderList.add(order))
  }

  def getUnprocessedOrders(): Iterable[Order] = {
    (buyOrdersByStockMap.map(_._2.toList) ++ sellOrdersByStockMap.map(_._2.toList)).flatten
  }
}
