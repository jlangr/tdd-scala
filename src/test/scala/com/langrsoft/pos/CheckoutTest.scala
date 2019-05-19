package com.langrsoft.pos

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{BeforeAndAfter, FunSpec, ShouldMatchers}
import spray.json._
import CheckoutJsonSupport._

class CheckoutTest extends FunSpec with ShouldMatchers with BeforeAndAfter with ScalatestRouteTest {
  val route = CheckoutRoutes.routes()

  before {
//    println("BEFORE RUNNING")
//    Post("/checkouts/clear") ~> route ~> check {}
  }

  describe("checkouts") {
    it("returns created checkout by ID") {
      Post("/checkouts/clear") ~> route ~> check {}
      val id = postCheckout

      Get(s"/checkouts?id=${id}") ~> route ~> check {
        status.isSuccess() shouldBe(true)

        val checkout: Checkout = responseAs[String].parseJson.convertTo[Checkout]
        checkout.memberId shouldEqual("member #1")
      }
    }

    it("returns all created checkouts") {
      Post("/clearcheckouts") ~> route ~> check {}
      val id1 = postCheckout
      val id2 = postCheckout

      Get(s"/allCheckouts") ~> route ~> check {
        status.isSuccess() shouldBe(true)

        println("checkouts:", responseAs[String])
        val checkoutsJson =
          responseAs[String].parseJson.asInstanceOf[JsArray].elements.map(_.convertTo[Checkout])
        checkoutsJson.length shouldEqual(2)
        checkoutsJson(0).memberId shouldEqual(s"member #${id1}")
        checkoutsJson(1).memberId shouldEqual(s"member #${id2}")
      }
    }
  }

  private def postCheckout = {
    Post("/checkouts", "") ~> route ~> check {
      responseAs[String]
    }
  }
}
