package lambdas
package tfdb
package semantics

import org.scalatest._

class TermSpec extends FunSpec with Matchers {

  val E = Examples[Term]
  import E._

  it("with free variables") {
    ex1[Function1]: (((Int => Int, (Int, Unit))) => Int)
    ex1[Function1].apply((identity, (4, ()))) shouldBe 4
  }
}
