package lambdas
package trees

import lambdas.arithmetic._
import org.scalatest._
class TreeSpec extends FunSpec with Matchers {

  import ArrowType.Constructors._
  import arithmetic.IntType.Constructors._
  import arithmetic.semantics.ArithmeticSerializer.Constructors._
  import tfhoas.semantics.Serializer.Constructors._

  tr_lam("x0", tr_tArr(tr_tInt, tr_tInt), tr_app(tr_vr("x0"), tr_int(1.bd))) shouldBe
  Node(
    "Lam",
    List(
      Leaf("x0"),
      Node("TArr", List(Leaf("TInt"), Leaf("TInt"))),
      Node("App", List(Node("Var", List(Leaf("x0"))), Node("Int", List(Leaf("1")))))
    )
  )
}
