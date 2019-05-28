package com.langrsoft.util

import org.joda.time.DateTime

import scala.collection.mutable

class Portfolio(var stockService: StockService) {
  val auditor: Auditor = new FSAuditor()

  // either do mutable.map or do a var
  val symbols = mutable.Map[String, Integer]()

  def isEmpty = size == 0

  def size = symbols.size

  def purchase(symbol: String, sharesToBuy: Integer) =
    sharesToBuy match {
      case n if n > 0 =>
        symbols += symbol -> (sharesToBuy + shares(symbol))
//        auditor.audit(s"Purchased $sharesToBuy shares of $symbol", new DateTime())
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
      (total, symbol) => total + stockService.price(symbol) * shares(symbol)
    }
}
//object ProductionPortfolio extends Portfolio {
//  override val stockService = new NASDAQStockService()
//}
