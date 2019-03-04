package lambdas
package taglessfinal
package debruijn
package semantics

import org.scalatest._

class TermSpec extends FunSpec with Matchers{

  // import Term.TermLambda._
  val E = Examples[Term]
  import E._

  ex: Term[Unit, (Int => Int) => Int]

  it("Show interpretation"){
    ex[ShowB].apply(0) shouldBe "(λx0.(x0 1))"
  }

  it("Eval interpretation"){
    ex.apply[Function1]: Function1[Unit, (Int => Int) => Int] // .apply(()) : (Int => Int) => Int
    ex.apply[Function1].apply(()).apply(_ + 1) shouldBe 2
  }

  it("with free variables"){
    ex1[Function1] : (((Int => Int, (Int, Unit))) => Int)
    ex1[Function1].apply((identity, (4, ()))) shouldBe 4
  }

}
