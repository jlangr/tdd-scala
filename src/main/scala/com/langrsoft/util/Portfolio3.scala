package com.langrsoft.util

import scala.collection.mutable

class Portfolio3 {
  this: StockServiceComponent => // "self type" that allows to mix-in a stock service

  val symbols = mutable.Map[String, Integer]()

  def isEmpty = size == 0

  def size = symbols.size

  def purchase(symbol: String, sharesToBuy: Integer) =
    sharesToBuy match {
      case n if n > 0 =>
        symbols += symbol -> (sharesToBuy + shares(symbol))
      case _ => throw new InvalidPurchaseException
    }

  def shares(symbol: String): Integer = symbols.getOrElse(symbol, 0)

  def value =
    symbols.keysIterator.foldLeft(0) {
      (total, symbol) =>
        total + shares(symbol) * service.price(symbol)
    }
}
