package com.langrsoft.util

class NASDAQStockService() extends StockService {
  override def price(symbol: String): Int = throw new RuntimeException("currently down")
}
