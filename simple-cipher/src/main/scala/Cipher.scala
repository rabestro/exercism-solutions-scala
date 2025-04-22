import Cipher.randomKey

import scala.util.Random

class Cipher(cipherKey: Option[String]):
  val key: String = cipherKey getOrElse randomKey
  val keyStream: Seq[Char] = LazyList.continually(key).flatten

  def encode(text: String): String =
    text.to(LazyList).zip(keyStream).map(Cipher.encode).mkString

  def decode(text: String): String =
    text.to(LazyList).zip(keyStream).map(Cipher.decode).mkString

object Cipher:
  private val MinKeyLength = 100
  private val MaxKeyLength = 1000000
  private val lettersInAlphabet = 'z' - 'a' + 1

  def randomKey: String =
    val keyLength = Random.between(MinKeyLength, MaxKeyLength)
    Random.alphanumeric.filter(_.isLower).take(keyLength).mkString

  def encode(symbol: Char, key: Char): Char =
    val keyIndex = key - 'a'
    val symbolIndex = symbol - 'a'
    val encodedSymbol = (symbolIndex + keyIndex) % lettersInAlphabet + 'a'
    encodedSymbol.toChar

  def decode(symbol: Char, key: Char): Char =
    val keyIndex = key - 'a'
    val symbolIndex = symbol - 'a'
    val encodedSymbol = (symbolIndex - keyIndex) % lettersInAlphabet + 'a'
    encodedSymbol.toChar
  