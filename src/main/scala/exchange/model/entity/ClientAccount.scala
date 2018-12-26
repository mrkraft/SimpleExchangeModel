package exchange.model.entity

import exchange.model.Exchange
import exchange.model.entity.ClientAccount.Stock

/**
  * Класс c информации о балансах на счетах клиентов
  */
case class ClientAccount(id: String, var dollarAmount: Int, stockBalances: Map[Char, Stock]) {

  def isMoneyEnough(tradeAmount: Int): Boolean =
    tradeAmount <= dollarAmount

  def isStocksEnough(stockId: Char, tradeAmount: Int): Boolean =
    stockBalances.get(stockId).map(_.amount >= tradeAmount).getOrElse(false)

  def addDollars(changeAmount: Int): Unit =
    dollarAmount += changeAmount

  /**
    * Имитация блокировки денег на счету
    */
  def blockDollars(tradeAmount: Int): Unit =
    dollarAmount -= tradeAmount

  def addStocks(stockId: Char, changeAmount: Int): Unit =
    stockBalances.get(stockId).map(_.amount += changeAmount)

  /**
    * Имитация блокировки ценных бумаг на счету
    */
  def blockStocks(stockId: Char, tradeAmount: Int): Unit =
    stockBalances.get(stockId).map(_.amount -= tradeAmount)

  override def toString: String = {
    s"$id\t$dollarAmount\t${stockBalances.map(_._2).toList.sortBy(_.id).map(_.amount).mkString("\t")}"
  }
}

object ClientAccount {

  def apply(params: Iterable[String]): ClientAccount = {
    val (idAndDollars, stocks) = params.splitAt(2)

    new ClientAccount(
      id = idAndDollars.head,
      dollarAmount = idAndDollars.last.toInt,
      stockBalances =
        Exchange.STOCK_LIST
          .zip(stocks)
          .map {
            case (id, amount) => (id, Stock(id, amount.toInt))
          }.toMap
    )
  }

  case class Stock(id: Char, var amount: Int)
  
  sealed trait AccountChanges
  case class DollarAccountChanges(clientId: String, amount: Int) extends AccountChanges
  case class StockAccountChanges(clientId: String, stockId: Char, amount: Int) extends AccountChanges
}
