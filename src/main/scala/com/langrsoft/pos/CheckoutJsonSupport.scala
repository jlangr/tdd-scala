package com.langrsoft.pos

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

case class Item(upc: String, description: String, price: BigDecimal)

case class Checkout(id: String, var memberId: String, var items: List[Item])

object CheckoutJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val anItem = jsonFormat3(Item)

  implicit object CheckoutJsonFormat extends RootJsonFormat[Checkout] {
    def write(checkout: Checkout) = JsObject(Map(
      "id" -> JsString(checkout.id),
      "memberId" -> JsString(checkout.memberId),
      "items" -> JsArray(checkout.items.map(_.toJson).toVector)
    ))

    def read(value: JsValue): Checkout = {
      value.asJsObject.getFields("id", "memberId", "items") match {
        case Seq(JsString(id), JsString(description), JsArray(items)) =>
          new Checkout(id, description, items.map(_.convertTo[Item]).to[List])
      }
    }
  }
}
