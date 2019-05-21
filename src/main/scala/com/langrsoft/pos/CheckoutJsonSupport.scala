package com.langrsoft.pos

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

case class Member(id: String, phoneNumber: String, name: String, discount: BigDecimal)

case class Item(id: String, upc: String, description: String, price: BigDecimal)

case class Checkout(id: String, var items: List[Item], var member: Member)

object CheckoutJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val aMember = jsonFormat4(Member)

  implicit val anItem = jsonFormat4(Item)

  implicit object CheckoutJsonFormat extends RootJsonFormat[Checkout] {
    def write(checkout: Checkout) = {
      var fields: Map[String,JsValue] = null
      fields = Map(
        "id" -> JsString(checkout.id),
        "items" -> JsArray(checkout.items.map(_.toJson).toVector))
      if (checkout.member != null) fields += ("member" -> checkout.member.toJson)
      JsObject(fields)
    }

    def read(value: JsValue): Checkout = {
      value.asJsObject.getFields("id", "items", "member") match {
        case Seq(JsString(id), JsArray(items), member) =>
          new Checkout(id, items.map(_.convertTo[Item]).to[List], member.convertTo[Member])
        case Seq(JsString(id), JsArray(items)) =>
          new Checkout(id, items.map(_.convertTo[Item]).to[List], null)
      }
    }
  }
}
