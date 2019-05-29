package com.langrsoft.util

trait ProdStockServiceComponent extends StockServiceComponent {
  val service = new ProdPriceService()

  class ProdPriceService() extends PriceService {
    def price(symbol: String): Int = { throw new RuntimeException("pricing unavailable!") }
  }
}
