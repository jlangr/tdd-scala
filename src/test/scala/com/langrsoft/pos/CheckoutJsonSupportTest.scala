package com.langrsoft.pos

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{BeforeAndAfter, FunSpec, ShouldMatchers}
import spray.json._
import CheckoutJsonSupport._

class CheckoutJsonSupportTest extends FunSpec with ShouldMatchers with BeforeAndAfter with ScalatestRouteTest {
  describe("checkout JSON support") {
    val itemJson = "{\"id\":\"1\",\"upc\":\"444\",\"description\":\"Eggs\",\"price\":4.44}"
    val checkoutJson = "{" +
      "\"id\":\"42\"," +
      "\"memberId\":\"719-287-GEEK\"," +
      "\"member\":{" +
      "\"id\":\"42\",\"phoneNumber\":\"719-287-4335\",\"name\":\"Jeff Languid\",\"discount\":0.1" +
      "}," +
      "\"items\":[" +
      "{\"id\":\"1\",\"upc\":\"111222333\",\"description\":\"milk\",\"price\":4.98}" +
      "]}"

    it("decodes a checkout") {
      val checkout = checkoutJson.parseJson.convertTo[Checkout]

      checkout.id shouldEqual("42")
      checkout.memberId shouldEqual("719-287-GEEK")
      val item = checkout.items(0)
      item.description shouldEqual("milk")
    }

    val noMemberCheckout = "{" +
      "\"id\":\"42\"," +
      "\"memberId\":\"719-287-GEEK\"," +
      "\"items\":[]" +
      "}"

    it("decodes null member") {
      val checkout = noMemberCheckout.parseJson.convertTo[Checkout]
      checkout.member shouldBe null
    }

    it("encodes null member") {
      val checkout = Checkout("42", "719-287-GEEK", null, List())
      checkout.toJson.toString shouldEqual(noMemberCheckout)
    }

    it("encodes a checkout") {
      val checkout = Checkout("42", "719-287-GEEK",
        Member("42", "719-287-4335", "Jeff Languid", BigDecimal(0.1)),
        List(Item("1", "111222333", "milk", BigDecimal(4.98))))

      checkout.toJson.toString shouldEqual(checkoutJson)
    }

    it("encodes an item") {
      val item = Item("1", "444", "Eggs", BigDecimal(4.44))

      item.toJson.toString shouldEqual(itemJson)
    }

    it("decodes an item") {
      val item = itemJson.parseJson.convertTo[Item]

      item.id shouldEqual("1")
      item.upc shouldEqual("444")
      item.description shouldEqual("Eggs")
      item.price shouldEqual(BigDecimal(4.44))
    }
  }
}
