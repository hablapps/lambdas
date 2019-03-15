package lambdas
package trees
package semantics

import safecast._
import syntax._

import org.scalatest._

class AdHocParseTermSpec extends FunSpec with Matchers with Inside {

  it("Int literals") {
    inside(AdHocParseTerm[ShowB, Unit, Unit](()).apply(tr_int(1))) {
      case Right(DynLTerm(_, term)) =>
        term(0) shouldBe "1"
    }
  }

  it("Add expressions") {
    inside(AdHocParseTerm[ShowB, Unit, Unit](()).apply(tr_add(tr_int(1), tr_int(2)))) {
      case Right(DynLTerm(_, term)) =>
        term(0) shouldBe "(1+2)"
    }
  }

  it("Lambda expressions") {
    inside(AdHocParseTerm[ShowB, Unit, Unit](()).apply(tr_lam("v0", tr_tInt, tr_vr("v0")))) {
      case Right(DynLTerm(_, term)) =>
        term(0) shouldBe "(λx0.x0)"
    }
  }

  it("App expressions") {
    inside(
      AdHocParseTerm[ShowB, Unit, Unit](())
        .apply(tr_app(tr_lam("v0", tr_tInt, tr_vr("v0")), tr_int(1)))
    ) {
      case Right(DynLTerm(_, term)) =>
        term(0) shouldBe "((λx0.x0) 1)"
    }
  }

  it("Poly expressions") {
    import tfdb.semantics.Term

    inside(
      AdHocParseTerm[Term, Unit, Unit](())
        .apply(tr_app(tr_lam("v0", tr_tInt, tr_vr("v0")), tr_int(1)))
    ) {
      case Right(DynLTerm(_, term)) =>
        term[ShowB].apply(0) shouldBe "((λx0.x0) 1)"
        term[Function1].apply(()) shouldBe 1
    }
  }

  it("Poly expressions (open)") {
    import tfdb.semantics.Term

    inside(
      AdHocParseTerm[Term, (Gamma.Var[Int], Unit), (Int, Unit)](
        (Gamma.Var("v0", tint[TypeTerm]), ())
      ).apply(tr_add(tr_vr("v0"), tr_int(1)))
    ) {

      case Right(DynLTerm(_, term)) =>
        // term[ShowB].apply(0) shouldBe "((λx0.x0) 1)"
        term[Function1].apply((1, ())) shouldBe 2
    }
  }
}
