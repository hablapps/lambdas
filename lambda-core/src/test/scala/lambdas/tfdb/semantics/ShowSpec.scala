package lambdas
package tfdb
package semantics

import arithmetic._

import org.scalatest._

class ShowSpec extends FunSpec with Matchers {

  val E = Examples[IntArrowType, ShowB]()
  val L = Lambda[IntArrowType, ShowB]
  import E._
  import L._

  describe("Show lambda expressions - tagless final - de bruijn") {
    it("Closed expressions - no lambdas") {
      //(add(int(1), int(3)))(0) shouldBe "(1+3)"
    }

    it("Closed expressions - with lambdas") {
      (lam(vz[Unit, Num])).apply(0) shouldBe "(λx0.x0)"
      (lam[Unit, Num, Num => Num](lam[Unit, Num, Num](vs[Unit, Num, Num](vz[Unit, Num]))))
        .apply(0) shouldBe "(λx0.(λx1.x0))"
      ex2(0) shouldBe "(λx0.(λx1.(x0 x1)))"
    }

    it("Open expressions") {
      //(add(vz, vs(vz)))(0) shouldBe "(y1+y2)"
      (lam(vs[Unit, Num, Num](vz[Unit, Num]))).apply(0) shouldBe "(λx0.y1)"
      //(lam(add(vz, vs(vz))))(0) shouldBe "(λx0.(x0+y1))"
      // ex4(0) shouldBe "(x0+x1)"
      ex1(0) shouldBe "(y2 y1)"
    }
  }
}
