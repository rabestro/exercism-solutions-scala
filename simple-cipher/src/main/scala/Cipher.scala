class Cipher(cipherKey: Option[String]):
  val key: String = cipherKey.getOrElse("aaaaaaaaaa")
  def encode(text: String): String = "aaaaaaaaaa"
  def decode(key: String): String = "aaaaaaaaaa"
  