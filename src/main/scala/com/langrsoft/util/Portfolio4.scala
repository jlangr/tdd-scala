package com.langrsoft.util

import scala.util.Try

case class PortfolioData(shares: Map[String,Int]=Map())

object Portfolio4 {
  def isEmpty(data: PortfolioData) = data.shares.isEmpty

  def uniqueSymbolCount(data: PortfolioData) = data.shares.size

  private def throwOnInvalidShareCount(sharesToBuy: Int) =
    if (sharesToBuy <= 0) throw new InvalidPurchaseException

  def purchase(data: PortfolioData, symbol: String, sharesToBuy: Int) = {
    throwOnInvalidShareCount(sharesToBuy)
    PortfolioData(data.shares + (symbol -> (shares(data, symbol) + sharesToBuy)))
  }

  def shares(data: PortfolioData, symbol: String): Int =
    data.shares.getOrElse(symbol, 0)

  def portfolioValue(data: PortfolioData, stockService: StockService) =
    data.shares.keysIterator.foldLeft(0) {
      (total, symbol) => {
        val sharePrice = Try(stockService.price(symbol)).getOrElse(0)
        total + (shares(data, symbol) * sharePrice)
      }
    }
}
