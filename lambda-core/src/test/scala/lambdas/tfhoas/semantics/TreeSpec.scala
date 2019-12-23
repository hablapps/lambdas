package lambdas
package tfhoas
package semantics

import org.scalatest._
import trees._
import TreeSerializable.ShowTree
import lambdas.arithmetic.Num

class TreeSpec extends FunSpec with Matchers {

  val L = Lambda[IntArrowType, ShowTree]
  import L._

  describe("Serialize lambda expressions") {

    it("works") {

      lam[Num, Num] { x0: ShowTree[Num] =>
        x0
      }.apply(0) shouldBe
      Node(
        "Lam",
        List(
          Leaf("x0"),
          Leaf("TInt"), // x0: Int
          Node("Var", List(Leaf("x0")))
        )
      ) // x0
    }
  }
}
