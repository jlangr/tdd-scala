package com.langrsoft.util

class NASDAQStockService() extends StockService {
  override def price(symbol: String): Integer = throw new RuntimeException("currently down")
}
