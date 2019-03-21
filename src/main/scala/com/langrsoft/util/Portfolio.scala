package com.langrsoft.util

import scala.collection.mutable.HashMap

class Portfolio {
  var symbols = HashMap[String, Integer]()

  def value: Integer = 0

  def isEmpty = size == 0

  def size = symbols.size

  def purchase(symbol: String, sharesToBuy: Integer) = {
    if (sharesToBuy <= 0) throw new InvalidPurchaseException
    symbols += symbol -> (sharesToBuy + shares(symbol))
  }

  def shares(symbol: String): Integer = {
    if (!symbols.contains(symbol))
      0
    else
      symbols(symbol)
  }
}
