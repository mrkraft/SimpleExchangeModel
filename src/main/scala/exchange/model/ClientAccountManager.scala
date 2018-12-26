package exchange.model

import exchange.model.Exchange._
import exchange.model.entity.ClientAccount.{AccountChanges, DollarAccountChanges, StockAccountChanges}
import exchange.model.entity.{ClientAccount, OrderInfo}
import exchange.util.DBUtils

/**
  * Класс для управления балансами аккаунтов клиентов
  */
class ClientAccountManager(clientAccounts: Map[String, ClientAccount]) {

  /**
    * Проверяет возможность создания заявки на бирже в зависимости от баланса.
    * Если баланса хватает, блокирует деньги или акции аккаунта и возвращает описание заявки для дальнейшей обработки.
    */
  def checkBalanceAndProcess(orderInfo: OrderInfo): Option[OrderInfo] = {
    val clientAccount = clientAccounts.get(orderInfo.clientId)
    clientAccount.flatMap {
      account =>

        orderInfo.operation match {
          case Operation.s =>
            if (account.isStocksEnough(orderInfo.stockId, orderInfo.stockAmount)) {
              account.blockStocks(orderInfo.stockId, orderInfo.stockAmount)
              Some(orderInfo)
            } else {
              None
            }

          case Operation.b =>
            if (account.isMoneyEnough(orderInfo.getFullPrice())){
              account.blockDollars(orderInfo.getFullPrice())
              Some(orderInfo)
            } else {
              None
            }
        }
    }
  }

  /**
    * Увеличивает балансы аккаунта.
    * Используется после исполнения заявки или в случае завершенияработы биржи, для разблокировки средств
    * не исполненых заявок.
    */
  def processAccountChanges(changes: Iterable[AccountChanges]): Unit = {
    changes.foreach {
      case DollarAccountChanges(clientId, amount) => clientAccounts.get(clientId).map(_.addDollars(amount))
      case StockAccountChanges(clientId, stockId, amount) => clientAccounts.get(clientId).map(_.addStocks(stockId, amount))
    }
  }

  /**
    * Сохраняет текущее состояние аккаунтов клиентов в файл
    */
  def saveClientAccountData(): Unit = DBUtils.saveClientAccountData(clientAccounts.map(_._2))

  def getClientAccountsList(): Iterable[ClientAccount] = {
    clientAccounts.values
  }
}
