package com.langrsoft.pos

import java.math.MathContext

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import CheckoutJsonSupport._

import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.RoundingMode

// This trait does a couple or more core things. How might you split it up?
trait CheckoutRoutes {
  val checkouts: ListBuffer[Checkout] = ListBuffer[Checkout]()
  val itemDatabase: Inventory
  val memberDatabase: MemberDatabase

  def nextId() = { checkouts.length + 1 }

  def routes() : Route = {
    concat(pathPrefix("checkouts") {
      concat(
        path(Segment / "items") {
          checkoutId => post { postItem(checkoutId) }
        },
        path(Segment / "member") {
          checkoutId => post { postMember(checkoutId) }
        },
        path(Segment / "total") {
          checkoutId => get { getTotal(checkoutId) } ~ post { postTotal(checkoutId)}
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
    fulfillCheckoutRequest(checkoutId, completeAttachMember)
  }

  private def postItem(checkoutId: String) = {
    fulfillCheckoutRequest(checkoutId, completeAddItem)
  }

  private def getTotal(checkoutId: String) = {
    fulfillCheckoutRequest(checkoutId, completeGetTotal)
  }

  private def postTotal(checkoutId: String) = {
    fulfillCheckoutRequest(checkoutId, completeTotal)
  }

  private def fulfillCheckoutRequest(checkoutId: String, completeFn: Checkout => Route) = {
    findCheckout(checkoutId) map completeFn getOrElse completeCheckoutNotFound(checkoutId)
  }

  val LineWidth = 45

  private def createReceipt(retrievedCheckout: Checkout) = {
    var total = BigDecimal(0)
    var totalOfDiscountedItems = BigDecimal(0)
    var totalSaved = BigDecimal(0)

    val lineItems = ListBuffer[String]()

    retrievedCheckout.items
      .foreach(item => {
        if (isDiscountable(item) && memberDiscount(retrievedCheckout) > 0) {
          val discountAmount = memberDiscount(retrievedCheckout) * item.price
          val discountedPrice = item.price * (1.0 - memberDiscount(retrievedCheckout))

          lineItems += createLineItem(item.price, item.description)
          lineItems += createLineItem(-discountAmount, s"   ${formatPercent(memberDiscount(retrievedCheckout))}% mbr disc")

          totalOfDiscountedItems += discountedPrice
          total += discountedPrice
          totalSaved += discountAmount
        } else {
          lineItems += createLineItem(item.price, item.description)

          total += item.price
        }
      })

    lineItems += createLineItem(total, "TOTAL")
    if (totalSaved > 0)
      lineItems += createLineItem(totalSaved, "*** You saved:")

    Receipt(round2(total), round2(totalSaved), round2(totalOfDiscountedItems), lineItems.toList)
  }

  private def memberDiscount(retrievedCheckout: Checkout) = {
    val memberDiscountAmount = if (hasMember(retrievedCheckout)) BigDecimal(0) else retrievedCheckout.member.get.discount
    memberDiscountAmount
  }

  private def hasMember(retrievedCheckout: Checkout) = {
    retrievedCheckout.member.isEmpty
  }

  private def isDiscountable(item: Item) = {
    !item.isExemptFromDiscount
  }

  private def createLineItem(totalSaved: BigDecimal, messageText: String) = {
    val formattedTotal = round2(totalSaved)
    val formattedTotalWidth = formattedTotal.toString.length
    val textWidth = LineWidth - formattedTotalWidth
    val message = pad(messageText, textWidth) + formattedTotal
    message
  }

  private def round2(price: BigDecimal) = {
    price.setScale(2, RoundingMode.HALF_EVEN)//.toString
  }

  private def formatPercent(discount: BigDecimal) = {
    (discount * 100).round(new MathContext(0)).toInt
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
    val checkout = Checkout(nextId().toString(), List(), Receipt(), None)
    checkouts += checkout
    complete(StatusCodes.Created, checkout.id)
  }

  private def findCheckout(checkoutId: String) = {
    checkouts.toList.find(checkout => checkout.id == checkoutId)
  }

  private def completeGetTotal(retrievedCheckout: Checkout) : Route = {
    val discount = retrievedCheckout.member map (member => member.discount) getOrElse BigDecimal(0)
    // could do group-by then reduce both...
    val total = retrievedCheckout.items.foldLeft(BigDecimal(0))(sumTotal(discount))
    complete(StatusCodes.Accepted, total.setScale(2).toString())
  }

  private def sumTotal(discount: BigDecimal) = {
    (total: BigDecimal, item: Item) =>
      val discountTo = if (item.isExemptFromDiscount) BigDecimal(1) else BigDecimal(1.0) - discount
      total + item.price * discountTo
  }

  private def completeTotal(checkout: Checkout) = {
    checkout.receipt = createReceipt(checkout)
    complete(StatusCodes.Accepted, checkout)
  }

  private def pad(s: String, length: Int) = {
    s + (" " * (length - s.length))
  }

  private def completeAddItem(retrievedCheckout: Checkout) = {
    entity(as[String]) { upc =>
      itemDatabase.retrieveItem(upc).map((item) => {
        retrievedCheckout.items = List.concat(retrievedCheckout.items, List(item))
        complete(StatusCodes.Accepted, item)
      })
      .getOrElse(completeUpcNotFound(upc))
    }
  }

  private def completeAttachMember(retrievedCheckout: Checkout) = {
    entity(as[String]) { phoneNumber =>
      memberDatabase.memberLookup(phoneNumber).map((member) => {
        retrievedCheckout.member = Some(member)
        complete(StatusCodes.Accepted)
      })
      .getOrElse(completeMemberNotFound(phoneNumber))
    }
  }

  private def completeCheckoutNotFound(checkoutId: String) = {
    complete(StatusCodes.NotFound, s"invalid checkout id: ${checkoutId}")
  }

  private def completeMemberNotFound(phoneNumber: String) = {
    complete(StatusCodes.NotFound, s"phone number not found: ${phoneNumber}")
  }

  private def completeUpcNotFound(upc: String) = {
    complete(StatusCodes.NotFound, s"invalid upc: ${upc}")
  }
}

object CheckoutRoutesImpl extends CheckoutRoutes {
  val itemDatabase = new Inventory()
  val memberDatabase = new MemberDatabase()
}
