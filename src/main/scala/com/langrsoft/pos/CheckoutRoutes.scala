package com.langrsoft.pos

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import CheckoutJsonSupport._

import scala.collection.mutable.ListBuffer

object CheckoutRoutes {
  var checkouts: ListBuffer[Checkout] = ListBuffer[Checkout]()

  def nextId() = {
    checkouts.length + 1
  }

  def routes() : Route = {
    path("checkouts") {
      post { createCheckout() } ~
      get { getCheckout() }
    } ~
    path("clearcheckouts") {
      post { clearAllCheckouts() }
    } ~
    path("allCheckouts") {
      get { getAllCheckouts() }
    }
  }

  def clearAllCheckouts() = {
    checkouts.clear()
    complete(StatusCodes.Accepted)
  }

  def getAllCheckouts(): Route = {
    complete(checkouts.toList)
  }

  def getCheckout() = {
    parameters('id.as[String]) { id => {
      val checkout: Option[Checkout] = checkouts.toList.find(checkout => checkout.id == id)
      complete(checkout.get)
    }}
  }

  def createCheckout() = {
    val checkout = Checkout(nextId().toString(), "", List())
    checkout.memberId = s"member #${checkout.id}"
    checkouts += checkout
    complete(StatusCodes.Created, checkout.id)
  }

//    entity(as[Checkout]) { entity =>
//    }
}

//  val stuff: concurrent.Future[String] = Unmarshal(entity).to[String]
