package exchange.util

import java.io.{File, PrintWriter}

import exchange.model.entity.{ClientAccount, OrderInfo}

import scala.collection.mutable.Queue
import scala.util.Properties

/**
  * Имитация механизма загрузки данных из хранилища
  */
object DBUtils {

  private val CLIENTS_FILE_NAME = "clients.txt"
  private val ORDERS_FILE_NAME = "orders.txt"
  private val RESULT_FILE_NAME = "result.txt"

  private def loadDataFromFile(fileName: String): Iterable[String] = {
    val source = scala.io.Source.fromFile(fileName)
    val data = try source.getLines().toList finally source.close()
    data
  }

  def loadClientAccountData(): Map[String, ClientAccount] = {
    val rawAccountData = loadDataFromFile(CLIENTS_FILE_NAME)

    rawAccountData
      .map(el => ClientAccount(el.split("\\t")))
      .map(accountData => accountData.id -> accountData)
      .toMap
  }

  def saveClientAccountData(clientAccounts: Iterable[ClientAccount]): Unit = {
    val pw = new PrintWriter(new File(RESULT_FILE_NAME))
    clientAccounts
      .toList
      .sortBy(_.id)
      .foreach(
        account =>
          pw.write(account.toString + Properties.lineSeparator)
      )

    pw.close
  }

  def loadClientOrderInfo(): Iterable[OrderInfo] = {
    val rawOrderInfo = loadDataFromFile(ORDERS_FILE_NAME)

    Queue[OrderInfo]() ++=
      rawOrderInfo
        .map(el => OrderInfo(el.split("\\t")))
  }
}
