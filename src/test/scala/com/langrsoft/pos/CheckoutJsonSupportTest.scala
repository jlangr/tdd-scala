package com.langrsoft.pos

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{BeforeAndAfter, FunSpec, ShouldMatchers}
import spray.json._
import CheckoutJsonSupport._

class CheckoutJsonSupportTest extends FunSpec with ShouldMatchers with BeforeAndAfter with ScalatestRouteTest {
  describe("checkout JSON support") {
    val itemJson = """{"id":"1","upc":"444","description":"Eggs","price":4.44}"""
    val checkoutJson = """{"id":"42","items":[{"id":"1","upc":"111222333","description":"milk","price":4.98}],"member":{"id":"42","phoneNumber":"719-287-4335","name":"Jeff Languid","discount":0.1}}"""
    val noMemberCheckoutJson = """{"id":"42","items":[]}"""

    it("decodes a checkout") {
      val checkout = checkoutJson.parseJson.convertTo[Checkout]

      checkout.id shouldEqual("42")
      val item = checkout.items(0)
      item.description shouldEqual("milk")
    }

    it("decodes null member") {
      val checkout = noMemberCheckoutJson.parseJson.convertTo[Checkout]
      checkout.member.isEmpty shouldBe true
    }

    it("encodes null member") {
      val checkout = Checkout("42", List(), None)
      checkout.toJson.toString shouldEqual noMemberCheckoutJson
    }

    it("encodes a checkout") {
      val checkout = Checkout("42",
        List(Item("1", "111222333", "milk", BigDecimal(4.98))),
        Some(Member("42", "719-287-4335", "Jeff Languid", BigDecimal(0.1)))
      )

      checkout.toJson.toString shouldEqual checkoutJson
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
