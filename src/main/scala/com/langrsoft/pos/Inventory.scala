package com.langrsoft.pos

class Inventory {
  def retrieveItem(upc: String): Option[Item] = {
    if (upc == "555")
      Some(Item("1", "555", "Rice Krispies", BigDecimal("3.79"), false))
    else
      Some(Item("2", "666", "Ritz Crackers", BigDecimal("3.29"), true))
  }
}
