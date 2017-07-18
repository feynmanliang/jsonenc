trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

object JsonEncoder {
  def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc

  def createEncoder[A](func: A => JsonValue): JsonEncoder[A] = new JsonEncoder[A] {
    def encode(value: A): JsonValue = func(value)
  }

  implicit val stringEncoder: JsonEncoder[String] =
    createEncoder(str => JsonString(str))
  implicit val doubleEncoder: JsonEncoder[Double] =
    createEncoder(num => JsonNumber(num))
  implicit val IntEncoder: JsonEncoder[Int] =
    createEncoder(num => JsonNumber(num))
  implicit val boolEncoder: JsonEncoder[Boolean] =
    createEncoder(bool => JsonBoolean(bool))

  implicit def listEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] =
    createEncoder(xs => JsonArray(xs.map(enc.encode)))

  implicit def optionEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    createEncoder(opt => opt.map(enc.encode).getOrElse(JsonNull))
}
