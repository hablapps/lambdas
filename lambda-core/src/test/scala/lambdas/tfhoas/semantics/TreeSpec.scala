package lambdas
package tfhoas
package semantics

import org.scalatest._

import trees._, TreeSerializable.ShowTree
import tfdbarith.IntArrowType

class TreeSpec extends FunSpec with Matchers {

  val L = Lambda[IntArrowType, ShowTree]
  import L._

  describe("Serialize lambda expressions") {

    it("works") {

      lam[Int, Int] { v =>
        v
      }.apply(0) shouldBe
      Node("Lam", List(Leaf("x0"), Leaf("TInt"), Node("Var", List(Leaf("x0")))))
    }
  }
}
