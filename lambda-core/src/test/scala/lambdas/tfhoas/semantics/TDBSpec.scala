package lambdas
package tfhoas
package semantics

import org.scalatest._

class TDBSpec extends FunSpec with Matchers {

  val H = Lambda[IntArrowType, TDB[IntArrowType, ?]]
  import H._

  // val E = Examples[IntArrowType, TDB[IntArrowType, ?]]
  // import E._
  import arithmetic._
  def ex2: TDB[IntArrowType, (Num => Num) => Num => Num] =
    lam((f: TDB[IntArrowType, Num => Num]) => lam((n: TDB[IntArrowType, Num]) => app(f)(n)))

  describe("Show lambda expressions - tagless final - de bruijn") {
    it("Closed expressions - with lambdas") {
      ex2.church[ShowB].apply(0) shouldBe "(λx0.(λx1.(x0 x1)))"
    }
  }
}
