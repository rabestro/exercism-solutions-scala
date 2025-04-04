import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


/** @version 1.1.0 */
class PrimeFactorsTest extends AnyFunSuite with Matchers {

  test("no factors") {
    PrimeFactors.factors(1) should be(List())
  }

  test("prime number") {
    PrimeFactors.factors(2) should be(List(2))
  }

  test("square of a prime") {
    PrimeFactors.factors(9) should be(List(3, 3))
  }

  test("cube of a prime") {
    PrimeFactors.factors(8) should be(List(2, 2, 2))
  }

  test("product of primes and non-primes") {
    PrimeFactors.factors(12) should be(List(2, 2, 3))
  }

  test("product of primes") {
    PrimeFactors.factors(901255) should be(List(5, 17, 23, 461))
  }

  test("factors include a large prime") {
    PrimeFactors.factors(93819012551L) should be(List(11, 9539, 894119))
  }
}
