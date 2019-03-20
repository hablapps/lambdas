package lambdas
package intArrowParser

import safecast._
import trees._, syntax._
import arithmetic.IntType
import tfdb.ArrowType, tfdb.semantics._

import org.scalatest._

class IntArrowTermParserSpec extends FunSpec with Matchers with Inside {

  it("Int literals") {
    inside(IntArrowTermParser[ShowB].apply(tr_int(1))(())) {
      case Right(DynLTerm(_, term)) =>
        term(0) shouldBe "1"
    }
  }

  it("Add expressions") {
    inside(IntArrowTermParser[ShowB].apply(tr_add(tr_int(1), tr_int(2)))(())) {
      case Right(DynLTerm(_, term)) =>
        term(0) shouldBe "(1+2)"
    }
  }

  it("Lambda expressions") {
    inside(IntArrowTermParser[ShowB].apply(tr_lam("v0", tr_tInt, tr_vr("v0")))(())) {
      case Right(DynLTerm(_, term)) =>
        term(0) shouldBe "(λx0.x0)"
    }
  }

  it("App expressions") {
    inside(
      IntArrowTermParser[ShowB]
        .apply(tr_app(tr_lam("v0", tr_tInt, tr_vr("v0")), tr_int(1)))(())
    ) {
      case Right(DynLTerm(_, term)) =>
        term(0) shouldBe "((λx0.x0) 1)"
    }
  }

  it("Poly expressions") {

    inside(
      IntArrowTermParser[IntArrowTerm]
        .apply(tr_app(tr_lam("v0", tr_tInt, tr_vr("v0")), tr_int(1)))(())
    ) {
      case Right(DynLTerm(_, term)) =>
        term[ShowB].apply(0) shouldBe "((λx0.x0) 1)"
        term[Function1].apply(()) shouldBe 1
    }
  }

  it("Poly expressions (open)") {

    inside(
      IntArrowTermParser[IntArrowTerm]
        .apply(tr_add(tr_vr("v0"), tr_int(1)))(
          (Gamma.Var("v0", IntType[IntArrowTypeTerm].tint), ())
        )
    ) {
      case Right(DynLTerm(_, term)) =>
        term[ShowB].apply(0) shouldBe "(y1+1)"
        term[Function1].apply((1, ())) shouldBe 2
    }
  }
}
