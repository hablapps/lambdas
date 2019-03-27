package lambdas
package tfdbarith

import trees._, Treeable.ShowTree

import org.scalatest._

class SerializerSpec extends FunSpec with Matchers with Inside {

  implicit val _ = tfhoast.semantics.Serializer._Lambda[IntArrowType]
  val E          = ExamplesHoas[IntArrowType, ShowTree]
  import E._

  import arithmetic.semantics.Serializer.Constructors._
  import arithmetic.IntType.Constructors._
  import ArrowType.Constructors._
  import tfhoast.semantics.Serializer.Constructors._

  describe("Serializing") {
    it("Lambda expressions") {
      `λf.(f 1)`(0) shouldBe
      tr_lam("x0", tr_tArr(tr_tint, tr_tint), tr_app(tr_vr("x0"), tr_int(1)))

      `λf.((f 1)+2)`(0) shouldBe
      tr_lam("x0", tr_tArr(tr_tint, tr_tint), tr_add(tr_app(tr_vr("x0"), tr_int(1)), tr_int(2)))

      `λx0.λx1.x0`(0) shouldBe
      tr_lam("x0", tr_tint, tr_lam("x1", tr_tint, tr_vr("x0")))
    }
  }
}
