package lambdas
package tfhoas
package semantics

import ProductType.Implicits._
import ArrowType.Implicits._
import arithmetic.IntType.Implicits._
import org.scalatest._
import arithmetic._

class DerivationHSpec extends FlatSpec with Matchers {

  val E = Examples[IntArrowType, Derivation]()(
    ArrowType[IntArrowType],
    ProductType[IntArrowType],
    IntType[IntArrowType],
    Lambda[IntArrowType, Derivation],
    Derivation.DerivationLambda[IntArrowType],
    null
  )

  "derivation" should "work" in {

    Derivation.show(E.ex4) shouldBe
    "ImplI(x0, AndI(AndE2(x0), AndE1(x0)))"
  }
}
