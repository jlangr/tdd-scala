package com.langrsoft.util

case class Portfolio6Data(symbols: Map[String,Int] = Map())

object Portfolio6 {
  def isEmpty(portfolio: Portfolio6Data) = count(portfolio) == 0

  def count(portfolio: Portfolio6Data) = portfolio.symbols.size

  def sharesOf(data: Portfolio6Data, symbol: String) = data.symbols.getOrElse(symbol, 0)

  def buy(portfolio: Portfolio6Data, symbol: String, shares: Int) = {
    transact(portfolio, symbol, shares)
  }

  def removeSymbolIfAllSold(portfolio: Portfolio6Data, symbol: String) = {
    if (sharesOf(portfolio, symbol) == 0)
      Portfolio6Data(portfolio.symbols - symbol)
    else
      portfolio
  }

  def throwOnOversell(portfolio: Portfolio6Data, symbol: String, shares: Int) = {
    if (shares > sharesOf(portfolio, symbol))
      throw new IllegalArgumentException
  }

  def sell(portfolio: Portfolio6Data, symbol: String, shares: Int) = {
    throwOnOversell(portfolio, symbol, shares)
    val portfolioAfterSale = transact(portfolio, symbol, -shares)
    removeSymbolIfAllSold(portfolioAfterSale, symbol)
  }

  private def transact(portfolio: Portfolio6Data, symbol: String, shares: Int) = {
    val newShares = sharesOf(portfolio, symbol) + shares
    Portfolio6Data(portfolio.symbols + (symbol -> newShares))
  }
}
