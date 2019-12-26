package lambdas
package tfhoas
package semantics

import org.scalatest._

class TDBSpec extends FunSpec with Matchers {

  val H =
    Lambda[IntArrowType, TDB[IntArrowType, ShowB, ?]](tfhoas.Lambda._TDBLambda[IntArrowType, ShowB])
  import H._

  // val E = Examples[IntArrowType, TDB[IntArrowType, ?]]
  // import E._
  import arithmetic._
  def ex2: TDB[IntArrowType, ShowB, (Num => Num) => Num => Num] =
    lam(
      (f: TDB[IntArrowType, ShowB, Num => Num]) =>
        lam((n: TDB[IntArrowType, ShowB, Num]) => app(f)(n))
    )

  describe("Show lambda expressions - tagless final - de bruijn") {
    it("Closed expressions - with lambdas") {
      ex2.closed.apply(0) shouldBe "(λx0.(λx1.(x0 x1)))"
    }
  }
}
