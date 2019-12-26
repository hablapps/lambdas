package lambdas
package tfdb
package semantics

import lambdas.arithmetic._
import org.scalatest._

class TermSpec extends FunSpec with Matchers {

  val E = Examples[IntArrowType, Term[IntArrowType, ?, ?]]
  import E._

  it("with free variables") {
    ex1[Function1]: (((Num, (Num => Num, Unit))) => Num)
    ex1[Function1].apply((4.bd, (identity, ()))) shouldBe 4.bd
  }
}
