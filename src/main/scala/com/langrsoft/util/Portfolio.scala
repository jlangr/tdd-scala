package com.langrsoft.util

class Portfolio {
  var symbols = collection.mutable.HashMap[String, Integer]()

  def isEmpty = { size == 0 }

  def size = {
    symbols.size
  }

  def purchase(symbol: String, sharesToBuy: Integer): Unit = {
    if (sharesToBuy <= 0) throw new InvalidPurchaseException
    symbols += symbol -> (sharesToBuy + shares(symbol))
  }

  def shares(symbol: String): Integer = {
    if (!symbols.contains(symbol)) return 0
    symbols(symbol)
  }
}
