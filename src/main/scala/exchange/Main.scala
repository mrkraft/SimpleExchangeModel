package exchange

import exchange.model.Exchange

object Main {

  def main(args: Array[String]): Unit = {

    val exchange = new Exchange()
    exchange.start()

    println("ALL DONE")
  }
}
