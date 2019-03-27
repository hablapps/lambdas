package lambdas
package tfhoast
package semantics

import org.scalatest._

import trees._, Treeable.ShowTree
import tfdbarith.IntArrowType

class TreeSpec extends FunSpec with Matchers {

  implicit def _IntArrowType = new Treeable[IntArrowType] {
    def show[A](t: IntArrowType[A]) =
      Leaf("TInt")
  }

  val L = Lambda[IntArrowType, ShowTree](Serializer._Lambda[IntArrowType])
  import L._

  implicit val _IntType = arithmetic.IntType[IntArrowType].tint

  describe("Serialize lambda expressions") {

    it("works") {

      lam[Int, Int] { v =>
        v
      }.apply(0) shouldBe
      Node("Lam", List(Leaf("x0"), Leaf("TInt"), Node("Var", List(Leaf("x0")))))
    }
  }
}
