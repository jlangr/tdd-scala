package com.langrsoft.util

import scala.collection.mutable
import scala.util.Try

import org.joda.time.DateTime

class AuditException extends RuntimeException

class Portfolio(var stockService: StockService, auditor: Option[Auditor] = None) {

  // either do mutable.map or do a var
  val symbols = mutable.Map[String, Integer]()

  def isEmpty = size == 0

  def size = symbols.size

  def purchase(symbol: String, sharesToBuy: Integer) =
    sharesToBuy match {
      case n if n > 0 =>
        symbols += symbol -> (sharesToBuy + shares(symbol))
        if (auditor.nonEmpty) {
          try {
            auditor.get.audit(s"buy: $sharesToBuy of $symbol", new DateTime)
          }
          catch {
            case _: RuntimeException => throw new AuditException()
          }
        }
      case _ => throw new InvalidPurchaseException
    }

  def shares(symbol: String): Integer =
    symbols.getOrElse(symbol, 0)

  def valueByHand(service: StockService) = {
    val soleSymbol = symbols.keySet.head
    service.price(soleSymbol)
  }

  def value =
    symbols.keysIterator.foldLeft(0) {
      (total, symbol) => {
        val sharePrice = Try(stockService.price(symbol)).getOrElse(0)
        total + (shares(symbol) * sharePrice)
      }
    }
}
//object ProductionPortfolio extends Portfolio {
//  override val stockService = new NASDAQStockService()
//}
