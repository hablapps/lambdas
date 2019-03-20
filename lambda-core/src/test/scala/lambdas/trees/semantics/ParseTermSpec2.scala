package lambdas
package trees
package semantics2

import safecast2._
import syntax._

import org.scalatest._

class TermParserSpec extends FunSpec with Matchers with Inside {

  it("Int literals") {
    inside(TermParser[ShowB].apply(tr_int(1))(())) {
      case Right(DynLTerm(_, term)) =>
        term(0) shouldBe "1"
    }
  }

  it("Add expressions") {
    inside(TermParser[ShowB].apply(tr_add(tr_int(1), tr_int(2)))(())) {
      case Right(DynLTerm(_, term)) =>
        term(0) shouldBe "(1+2)"
    }
  }

  it("Lambda expressions") {
    inside(TermParser[ShowB].apply(tr_lam("v0", tr_tInt, tr_vr("v0")))(())) {
      case Right(DynLTerm(_, term)) =>
        term(0) shouldBe "(λx0.x0)"
    }
  }

  it("App expressions") {
    inside(
      TermParser[ShowB]
        .apply(tr_app(tr_lam("v0", tr_tInt, tr_vr("v0")), tr_int(1)))(())
    ) {
      case Right(DynLTerm(_, term)) =>
        term(0) shouldBe "((λx0.x0) 1)"
    }
  }

  import tfdb.Lambda
  import arithmetic.Arithmetic

  trait ArithLambdaTerm[E, T] {
    def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, T]
  }

  object ArithLambdaTerm {
    implicit object ArithLambdaTermArith extends ForAll[ArithLambdaTerm, Arithmetic] {
      def apply[E] = new Arithmetic[ArithLambdaTerm[E, ?]] {
        def int(i: Int) = new ArithLambdaTerm[E, Int] {
          def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]) =
            A[E].int(i)
        }

        def add(i1: ArithLambdaTerm[E, Int])(i2: ArithLambdaTerm[E, Int]) =
          new ArithLambdaTerm[E, Int] {
            def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]) =
              A[E].add(i1(L, A))(i2(L, A))
          }

        def * = new ArithLambdaTerm[E, (Int, Int) => Int] {
          def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]) =
            A[E].*
        }

        def + = new ArithLambdaTerm[E, (Int, Int) => Int] {
          def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]) =
            A[E].+
        }

      }
    }

    implicit object ArithLambdaTermLambda extends Lambda[ArithLambdaTerm] {

      def int[E](i: Int) = new ArithLambdaTerm[E, Int] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, Int] =
          A[E].int(i)
      }

      def add[E](i1: ArithLambdaTerm[E, Int], i2: ArithLambdaTerm[E, Int]) =
        new ArithLambdaTerm[E, Int] {
          def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, Int] =
            A[E].add(i1(L, A))(i2(L, A))
        }

      def vz[E, T] = new ArithLambdaTerm[(T, E), T] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[(T, E), T] =
          L.vz
      }

      def vs[E, T, T1](a: ArithLambdaTerm[E, T]) = new ArithLambdaTerm[(T1, E), T] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[(T1, E), T] =
          L.vs(a(L, A))
      }

      def lam[E, T1, T2](t: ArithLambdaTerm[(T1, E), T2]) = new ArithLambdaTerm[E, T1 => T2] {
        def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, T1 => T2] =
          L.lam(t(L, A))
      }

      def app[E, T1, T2](f: ArithLambdaTerm[E, T1 => T2])(t1: ArithLambdaTerm[E, T1]) =
        new ArithLambdaTerm[E, T2] {
          def apply[P[_, _]](implicit L: Lambda[P], A: ForAll[P, Arithmetic]): P[E, T2] =
            L.app(f(L, A))(t1(L, A))
        }
    }
  }

  it("Poly expressions") {

    inside(
      TermParser[ArithLambdaTerm]
        .apply(tr_app(tr_lam("v0", tr_tInt, tr_vr("v0")), tr_int(1)))(())
    ) {
      case Right(DynLTerm(_, term)) =>
        term[ShowB].apply(0) shouldBe "((λx0.x0) 1)"
        term[Function1].apply(()) shouldBe 1
    }
  }

  it("Poly expressions (open)") {

    inside(
      TermParser[ArithLambdaTerm]
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
