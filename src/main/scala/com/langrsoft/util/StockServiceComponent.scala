package com.langrsoft.util

trait StockServiceComponent {
  val service: PriceService

  trait PriceService {
    def price(symbol: String): Int
  }
}
