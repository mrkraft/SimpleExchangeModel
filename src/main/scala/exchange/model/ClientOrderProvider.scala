package exchange.model

import exchange.model.entity.OrderInfo
import exchange.util.DBUtils

/**
  * Имитация потока заявок обычной биржи
  */
class ClientOrderProvider(processOrder: OrderInfo => Unit) {

  val clientOrderList: Iterable[OrderInfo] = DBUtils.loadClientOrderInfo()

  def sendOrders() = {
    clientOrderList.foreach(processOrder(_))
  }
}
