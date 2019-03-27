package lambdas
package tfdbarith

import trees._, TreeSerializable.ShowTree

import org.scalatest._

class SerializerSpec extends FunSpec with Matchers with Inside {

  val E = ExamplesHoas[IntArrowType, ShowTree]
  import E._

  import arithmetic.semantics.Serializer.Constructors._
  import arithmetic.IntType.Constructors._
  import ArrowType.Constructors._
  import tfhoas.semantics.Serializer.Constructors._

  describe("Serializing") {
    it("Lambda expressions") {
      `λf.(f 1)`(0) shouldBe
      tr_lam("x0", tr_tArr(tr_tInt, tr_tInt), tr_app(tr_vr("x0"), tr_int(1)))

      `λf.((f 1)+2)`(0) shouldBe
      tr_lam("x0", tr_tArr(tr_tInt, tr_tInt), tr_add(tr_app(tr_vr("x0"), tr_int(1)), tr_int(2)))

      `λx0.λx1.x0`(0) shouldBe
      tr_lam("x0", tr_tInt, tr_lam("x1", tr_tInt, tr_vr("x0")))
    }
  }
}
