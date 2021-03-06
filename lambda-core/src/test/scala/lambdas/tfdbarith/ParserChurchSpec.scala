package lambdas
package tfdbarith

import arithmetic.IntType
import trees._, tfdbparser._
import TreeSerializer._

import org.scalatest._

class ChurchParserSpec extends FunSpec with Matchers with Inside {

  describe("Deserializing to Church") {

    it("without variables") {
      inside(
        IntArrowParser[Church[IntArrowType, ?, ?]]
          .apply(tr_app(tr_lam("v0", tr_tInt, tr_vr("v0")), tr_int(1)))(())
      ) {
        case Right(DynLTerm(_, term)) =>
          term[ShowB].apply(0) shouldBe "((λx0.x0) 1)"
          term[Function1].apply(()) shouldBe 1
      }
    }

    it("with variables") {

      inside(
        IntArrowParser[Church[IntArrowType, ?, ?]].apply(tr_add(tr_vr("v0"), tr_int(1)))(
          (Gamma.Var("v0", IntType[IntArrowType].tint), ())
        )
      ) {
        case Right(DynLTerm(_, term)) =>
          term[ShowB].apply(0) shouldBe "(y1+1)"
          term[Function1].apply((1, ())) shouldBe 2
      }
    }
  }
}
