package exchange.model.entity

import exchange.model.Exchange.Operation._
import exchange.model.Exchange._
import exchange.model.entity.ClientAccount.{AccountChanges, DollarAccountChanges, StockAccountChanges}

/**
  * Класс для хранения информации о заявке, которая поступила на биржу
  */
case class OrderInfo(clientId: String, operation: Operation, stockId: Char, stockPrice: Int, stockAmount: Int) {

  def getFullPrice() = stockAmount * stockPrice

  /**
    * Проверка возможности матчинга с другой заявкой
    */
  def isMergeableWith(that: OrderInfo): Boolean = {
    this.operation != that.operation &&
      this.clientId != that.clientId &&
      this.stockId == that.stockId &&
      ((this.operation == Operation.s && this.stockPrice <= that.stockPrice) ||
        (this.operation == Operation.b && this.stockPrice >= that.stockPrice))
  }

  /**
    * Возвращает объект для разблокировки средств пользователя, которые были заблокированы для этой заявки
    */
  def getUnblockedAccountChange(): AccountChanges = {
    operation match {
      case Operation.s => StockAccountChanges(clientId, stockId, stockAmount)
      case Operation.b => DollarAccountChanges(clientId, getFullPrice())
    }
  }
}

object OrderInfo {

  def apply(params: Array[String]): OrderInfo = {
    new OrderInfo(
      clientId = params(0),
      operation = Operation.withName(params(1)),
      stockId = params(2).charAt(0),
      stockPrice = params(3).toInt,
      stockAmount = params(4).toInt
    )
  }
}
