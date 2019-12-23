package lambdas
package strings
package semantics

import cats.Id
import org.scalatest._
import arithmetic._

class StandardStringsSpec extends FunSpec with Matchers {

  val S = Strings[Id]

  describe("Standard strings") {
    it("should work") {
      S.string("hola") shouldBe "hola"
      S.length(S.string("hola")) shouldBe 4.bd
    }
  }
}
