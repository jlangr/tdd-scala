package com.langrsoft.pos

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{BeforeAndAfter, FunSpec, ShouldMatchers}
import spray.json._
import CheckoutJsonSupport._

class CheckoutJsonSupportTest extends FunSpec with ShouldMatchers with BeforeAndAfter with ScalatestRouteTest {
  describe("checkout JSON support") {
    val itemJson = "{\"upc\":\"444\",\"description\":\"Eggs\",\"price\":4.44}"
    val checkoutJson = "{" +
      "\"id\":\"42\"," +
      "\"memberId\":\"719-287-GEEK\"," +
      "\"items\":[" +
      "{\"upc\":\"111222333\",\"description\":\"milk\",\"price\":4.98}" +
      "]}"

    it("decodes a checkout") {
      val checkout = checkoutJson.parseJson.convertTo[Checkout]

      checkout.id shouldEqual("42")
      checkout.memberId shouldEqual("719-287-GEEK")
      val item = checkout.items(0)
      item.description shouldEqual("milk")
    }

    it("encodes a checkout") {
      val checkout = Checkout("42", "719-287-GEEK", List(Item("111222333", "milk", BigDecimal(4.98))))

      checkout.toJson.toString shouldEqual(checkoutJson)
    }

    it("encodes an item") {
      val item = Item("444", "Eggs", BigDecimal(4.44))

      item.toJson.toString shouldEqual(itemJson)
    }

    it("decodes an item") {
      val item = itemJson.parseJson.convertTo[Item]

      item.upc shouldEqual("444")
      item.description shouldEqual("Eggs")
      item.price shouldEqual(BigDecimal(4.44))
    }
  }
}
