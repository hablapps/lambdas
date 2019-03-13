package lambdas
package tfhoas
package semantics

import org.scalatest._

import arithmetic._

class ShowSpec extends FunSpec with Matchers {

  val A = Arithmetic[Show]
  import A._
  val L = Lambda[Show]
  import L._
  val E = Examples[Show]()
  import E._

  describe("Evaluate lambda expressions - tagless final - HOAS") {

    it("works") {

      ex1(0) shouldBe "1+3"

      // ex2[Unit]((3, ()))(4) shouldBe 7

      // ex4((3, (4, ()))) shouldBe 7

      ex3(0) shouldBe "(λx0.(x0 1)+2)"

      lam[Int, Int](x0 => add(x0)(int(1)))(0) shouldBe
      "(λx0.x0+1)"
    }
  }
}
