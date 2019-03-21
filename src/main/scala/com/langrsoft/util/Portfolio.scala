package com.langrsoft.util

import scala.collection.mutable.HashMap

class Portfolio {
  var auditor: Auditor = new FSAuditor()

  var stockService: StockService = new NASDAQStockService()

  var symbols = HashMap[String, Integer]()

  def isEmpty = size == 0

  def size = symbols.size

  def purchase(symbol: String, sharesToBuy: Integer) = {
    if (sharesToBuy <= 0) throw new InvalidPurchaseException
    symbols += symbol -> (sharesToBuy + shares(symbol))
    auditor.audit(s"Purchased $sharesToBuy shares of $symbol", null)
  }

  def shares(symbol: String): Integer = {
    if (!symbols.contains(symbol))
      0
    else
      symbols(symbol)
  }

  def value: Integer = {
    if (isEmpty)
      0
    else
      symbols.keysIterator.foldLeft(0) {
        (total, symbol) => total + stockService.price(symbol) * shares(symbol)
      }
  }
}
