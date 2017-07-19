package jsonenc

import org.scalatest._

import JsonEncoder._
import JsonObjectEncoder._

class JsonEncoderSpec extends FlatSpec with Matchers {
  "A JsonEncoder" should "encode case classes" in {
    case class IceCream(name: String, numCherries: Int, inCone: Boolean)

    val iceCream = IceCream("Sundae", 1, false)

    JsonEncoder[IceCream].encode(iceCream) shouldBe JsonObject(List(
      ("name", JsonString("Sundae")),
      ("numCherries", JsonNumber(1)),
      ("inCone", JsonBoolean(false)),
    ))
  }
}
