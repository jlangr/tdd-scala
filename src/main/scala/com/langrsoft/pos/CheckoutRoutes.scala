package com.langrsoft.pos

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import CheckoutJsonSupport._

import scala.collection.mutable.ListBuffer

trait CheckoutRoutes {
  var checkouts: ListBuffer[Checkout] = ListBuffer[Checkout]()
  val itemDatabase: Inventory
  val memberDatabase: MemberDatabase

  def nextId() = { checkouts.length + 1 }

  def routes() : Route = {
    concat(pathPrefix("checkouts") {
      concat(
        path(Segment / "items") {
          checkoutId => { post { postItem(checkoutId) } }
        },
        path(Segment / "member") {
          checkoutId => { post { postMember(checkoutId) } }
        },
        path("clear") {
          clearAllCheckouts()
        },
        post { postCheckout() },
        get { getCheckout() }
      )
    },
    path("allCheckouts") {
      get { getAllCheckouts() }
    })
  }

  private def postMember(checkoutId: String) = {
    findCheckout(checkoutId) match {
      case Some(retrievedCheckout) =>
        entity(as[String]) { phoneNumber =>
          memberDatabase.memberLookup(phoneNumber) match {
            case Some(member) =>
              val checkout: Checkout = retrievedCheckout
              checkout.member = Some(member)
              complete(StatusCodes.Accepted)
            case None =>
              complete(StatusCodes.NotFound, s"phone number not found: 719-287-4335")
          }
        }
      case None =>
        complete(StatusCodes.NotFound, s"invalid checkout id: ${checkoutId}")
    }
  }

  private def findCheckout(checkoutId: String) = {
    checkouts.toList.find(checkout => checkout.id == checkoutId)
  }

  private def postItem(checkoutId: String) = {
    findCheckout(checkoutId) match {
      case Some(retrievedCheckout) =>
        entity(as[String]) { upc =>
          itemDatabase.retrieveItem(upc) match {
            case Some(item) =>
              retrievedCheckout.items = List.concat(retrievedCheckout.items, List(item))
              complete(StatusCodes.Accepted, item)
            case None =>
              complete(StatusCodes.NotFound, s"invalid upc: ${upc}")
          }
        }
      case None =>
        complete(StatusCodes.NotFound, s"invalid checkout id: ${checkoutId}")
    }
  }

  private def clearAllCheckouts() = {
    checkouts.clear()
    complete(StatusCodes.Accepted)
  }

  private def getAllCheckouts(): Route = {
    complete(checkouts.toList)
  }

  private def getCheckout() = {
    parameters('id.as[String]) { id => complete(findCheckout(id).get)}
  }

  private def postCheckout() = {
    val checkout = Checkout(nextId().toString(), List(), None)
    checkouts += checkout
    complete(StatusCodes.Created, checkout.id)
  }
}

object CheckoutRoutesImpl extends CheckoutRoutes {
  val itemDatabase = new Inventory()
  val memberDatabase = new MemberDatabase()
}
//  val stuff: concurrent.Future[String] = Unmarshal(entity).to[String]
