package jsonenc

trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

/**
 * Encoder for Scala types. These are elements within the generic HList represents.
 */
object JsonEncoder {
  def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc

  def createEncoder[A](f: A => JsonValue): JsonEncoder[A] =
    new JsonEncoder[A] {
      def encode(value: A): JsonValue = f(value)
    }

  /* Instance encoders. */
  implicit val stringEncoder: JsonEncoder[String] =
    createEncoder(str => JsonString(str))
  implicit val doubleEncoder: JsonEncoder[Double] =
    createEncoder(num => JsonNumber(num))
  implicit val IntEncoder: JsonEncoder[Int] =
    createEncoder(num => JsonNumber(num))
  implicit val boolEncoder: JsonEncoder[Boolean] =
    createEncoder(bool => JsonBoolean(bool))

  /* Instance combinators. */
  implicit def listEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] =
    createEncoder(xs => JsonArray(xs.map(enc.encode)))

  implicit def optionEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    createEncoder(opt => opt.map(enc.encode).getOrElse(JsonNull))
}
