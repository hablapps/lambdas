package lambdas
package deserialization

import org.scalatest._

class TypecheckSpec extends FunSpec with Matchers with Inside{

  it("Int literals"){
    inside(Typecheck[ShowB, Unit, Unit](tr_int(1), ())){
      case Right(DynTerm(_, term)) =>
        term(0) shouldBe "1"
    }
  }

  it("Add expressions"){
    inside(Typecheck[ShowB, Unit, Unit](tr_add(tr_int(1), tr_int(2)), ())){
      case Right(DynTerm(_, term)) =>
        term(0) shouldBe "(1+2)"
    }
  }

  it("Lambda expressions"){
    inside(Typecheck[ShowB, Unit, Unit](tr_lam("v0", tr_tInt, tr_vr("v0")), ())){
      case Right(DynTerm(_, term)) =>
        term(0) shouldBe "(λx0.x0)"
    }
  }

  it("App expressions"){
    inside(Typecheck[ShowB, Unit, Unit](tr_app(tr_lam("v0", tr_tInt, tr_vr("v0")), tr_int(1)), ())){
      case Right(DynTerm(_, term)) =>
        term(0) shouldBe "((λx0.x0) 1)"
    }
  }

  it("Poly expressions"){
    import taglessfinal.debruijn.semantics.Term

    inside(Typecheck[Term, Unit, Unit](tr_app(tr_lam("v0", tr_tInt, tr_vr("v0")), tr_int(1)), ())){
      case Right(DynTerm(_, term)) =>
        term[ShowB].apply(0) shouldBe "((λx0.x0) 1)"
        term[Function1].apply(()) shouldBe 1
    }
  }

  it("Poly expressions (open)"){
    import taglessfinal.debruijn.semantics.Term

    inside(Typecheck[Term, (Gamma.VarDesc[Int], Unit), (Int, Unit)](
        tr_add(tr_vr("v0"), tr_int(1)), (Gamma.VarDesc("v0", tint[TQ]), ()))){
      case Right(DynTerm(_, term)) =>
        // term[ShowB].apply(0) shouldBe "((λx0.x0) 1)"
        term[Function1].apply((1, ())) shouldBe 2
    }
  }
}
