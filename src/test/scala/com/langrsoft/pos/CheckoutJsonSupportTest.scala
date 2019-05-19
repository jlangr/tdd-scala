package com.langrsoft.pos

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{BeforeAndAfter, FunSpec, ShouldMatchers}
import spray.json._
import CheckoutJsonSupport._

class CheckoutJsonSupportTest extends FunSpec with ShouldMatchers with BeforeAndAfter with ScalatestRouteTest {
  describe("checkout JSON support") {
    val json = "{" +
      "\"id\":\"42\"," +
      "\"memberId\":\"719-287-GEEK\"," +
      "\"items\":[" +
      "{\"upc\":\"111222333\",\"description\":\"milk\",\"price\":4.98}" +
      "]}"

    it("decodes") {
      val checkout = json.parseJson.convertTo[Checkout]
      checkout.id shouldEqual("42")
      checkout.memberId shouldEqual("719-287-GEEK")
      val item = checkout.items(0)
      item.description shouldEqual("milk")
    }

    it("encodes") {
      val checkout = Checkout("42", "719-287-GEEK", List(Item("111222333", "milk", BigDecimal("4.98"))))

      checkout.toJson.toString shouldEqual(json)
    }
  }
}
