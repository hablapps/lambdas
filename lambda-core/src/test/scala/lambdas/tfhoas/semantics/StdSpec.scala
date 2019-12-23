package lambdas
package tfhoas
package semantics

import org.scalatest._
import arithmetic._

class StdSpec extends FunSpec with Matchers {

  val L = Examples[IntArrowType, cats.Id]()
  import L._

  describe("Evaluate lambda expressions - tagless final - HOAS") {

    it("works") {

      ex1 shouldBe 4.bd

      ex3(_ + 1.bd) shouldBe 4.bd
    }
  }
}
