package lambdas
package intArrowParser

import trees._, tfdbparser._, arithparser._

import org.scalatest._

class ShowParserSpec extends FunSpec with Matchers with Inside {

  describe("Deserializing to Show") {
    it("Int literals") {
      inside(IntArrowParser[ShowB].apply(tr_int(1))(())) {
        case Right(DynLTerm(_, term)) =>
          term(0) shouldBe "1"
      }
    }

    it("Add expressions") {
      inside(IntArrowParser[ShowB].apply(tr_add(tr_int(1), tr_int(2)))(())) {
        case Right(DynLTerm(_, term)) =>
          term(0) shouldBe "(1+2)"
      }
    }

    it("Lambda expressions") {
      inside(IntArrowParser[ShowB].apply(tr_lam("v0", tr_tInt, tr_vr("v0")))(())) {
        case Right(DynLTerm(_, term)) =>
          term(0) shouldBe "(λx0.x0)"
      }
    }

    it("App expressions") {
      inside(
        IntArrowParser[ShowB]
          .apply(tr_app(tr_lam("v0", tr_tInt, tr_vr("v0")), tr_int(1)))(())
      ) {
        case Right(DynLTerm(_, term)) =>
          term(0) shouldBe "((λx0.x0) 1)"
      }
    }
  }
}
