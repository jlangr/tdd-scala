package com.langrsoft.util

case class PortfolioData(holdings: Map[String,Int] = Map())

object PortfolioF {
  def isEmpty(data: PortfolioData): Boolean = count(data) == 0

  def sharesOf(data: PortfolioData, symbol: String): Int =
    data.holdings.getOrElse(symbol, 0)

  def count(data: PortfolioData): Int = data.holdings.size

  def purchase(data: PortfolioData, symbol: String, shares: Int): PortfolioData = {
    if (shares <= 0) throw new IllegalArgumentException
    PortfolioData(data.holdings + (symbol -> (shares + sharesOf(data, symbol))))
  }
}
