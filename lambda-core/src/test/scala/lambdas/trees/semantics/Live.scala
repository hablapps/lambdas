package lambdas
package trees
package semantics

import scala.language.existentials
import safecast._
import syntax._

import org.scalatest._
import cats.Id

class Live extends FunSpec with Matchers with Inside {

  /*
   We start from a semi-structured serialization of lambda expressions.
   it could be JSON, XML, ...
   */

  val t1: Tree = tr_lam("x0", tr_tArr(tr_tInt, tr_tInt), tr_app(tr_vr("x0"), tr_int(1)))

  it("Trees are Json-like structures") {
    t1 shouldBe
    Node(
      "Lam",
      List(
        Leaf("x0"),
        Node("TArr", List(Leaf("TInt"), Leaf("TInt"))),
        Node("App", List(Node("Var", List(Leaf("x0"))), Node("Int", List(Leaf("1")))))
      )
    )
  }

  /**
  The goal is arriving at the following expressions.
    */
  def l1_hoas[P[_]](implicit L: tfhoas.Lambda[P]): P[(Int => Int) => Int] =
    L.lam { x0: P[Int => Int] =>
      L.app(x0)(L.int(1))
    }

  /**
  Allowing multiple potential evaluations
    */
  it("Printing lambda expressions (hoas)") {
    l1_hoas[Show].apply(0) shouldBe
    "(λx0.(x0 1))"
  }

  it("Evaluating lambda expressions (hoas)") {
    (l1_hoas[Id]: (Int => Int) => Int).apply(_ + 1) shouldBe
    2
  }

  /**
  But we can only deserialize values, not methods. We need a free algebra for Lambdas.
  We have two options for that: GADTs and Church encondings. We can't instantiate the HOAS
  algebra for the Church enconding, but we can do it for the De Bruijn version. This will
  simplify things.
    */
  def l1_db[P[_, _]](implicit L: tfdb.Lambda[P]): P[Unit, (Int => Int) => Int] =
    L.lam(L.app(L.vz[Unit, Int => Int])(L.int(1)))

  /**
  The environment of de bruijn lambda expressions allows us to represent open expressions,
  i.e. expressions with free variables.
    */
  def l2_db[P[_, _]](implicit L: tfdb.Lambda[P]): P[(Int => Int, (Int, Unit)), Int] =
    L.app(L.vz[(Int, Unit), Int => Int])(L.vs(L.vz[Unit, Int]))

  /**
  Interpreting lambda expressions, de bruijn style.
    */
  it("Evaluating lambda expressions (de bruijn)") {
    (l1_db[Function1]: Unit => (Int => Int) => Int).apply(()).apply(_ + 1) shouldBe
    2

    (l2_db[Function1]: ((Int => Int, (Int, Unit))) => Int).apply((_ + 1, (2, ()))) shouldBe
    3
  }

  it("Showing lambda expressions (de bruijn)") {
    (l1_db[ShowB]: Int => String).apply(0) shouldBe
    "(λx0.(x0 1))"

    (l2_db[ShowB]: Int => String).apply(0) shouldBe
    "(y1 y2)"
  }

  /*
  Church encoding is one of the possible interpretations of de bruijn algebra.
   */

  import tfdb.semantics.Term.{ TermLambda => T }

  val l1_db_c: tfdb.Term[Unit, (Int => Int) => Int] =
    T.lam(T.app(T.vz[Unit, Int => Int])(T.int(1)))

  def l2_db_c: tfdb.Term[(Int => Int, (Int, Unit)), Int] =
    T.app(T.vz[(Int, Unit), Int => Int])(T.vs(T.vz[Unit, Int]))

  // Alternatively

  val l1_db_c1: tfdb.Term[Unit, (Int => Int) => Int] =
    l1_db[tfdb.Term]

  def l2_db_c1: tfdb.Term[(Int => Int, (Int, Unit)), Int] =
    l2_db[tfdb.Term]

  /**
  The result of typechecking trees is an interpretation of the algebra, plus a type representation.
    */
  val Right(dt @ DynLTerm(typ, term)) = ParseTerm[Function1, Unit, Unit](()).apply(t1)

  it("Safe casts for dynamic terms") {
    // term: (Unit => _)
    // typ: TypeTerm[_]

    typ[λ[T => String]] shouldBe "((Int => Int) => Int)"

    inside(dt.as((tint[TypeTerm] -> tint[TypeTerm]) -> tint[TypeTerm])) {
      case Some(_) =>
      // _term: (Unit => ((Int => Int) => Int))
    }

    inside(dt.as(tint[TypeTerm] -> tint[TypeTerm])) {
      case None => ()
    }
  }
}
