package com.langrsoft.pos

class Inventory {
  def retrieveItem(upc: String): Option[Item] = {
    Some(Item("1", "333", "Milk", BigDecimal("3.33"), false))
  }
}
