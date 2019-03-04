package lambdas
package initial
package hoas
package semantics

import org.scalatest._

class ShowLambdaSpec extends FunSpec with Matchers{

  describe("Show lambda expressions - initial - HOAS"){
    it("works"){

      ShowLambda(IntL(1))(0) shouldBe
        "1"

      ShowLambda(Add(IntL(1), IntL(2)))(0) shouldBe
        "1+2"

      ShowLambda(Lam[ShowH,Int,Int](x0 => Add(x0, IntL(1))))(0) shouldBe
        "(λx0.x0+1)"

      ShowLambda(Lam{ x0: Lambda[ShowH, Int] => Add(x0, IntL(1))})(0) shouldBe
        "(λx0.x0+1)"

      ShowLambda[Int => Int](Lam(x0 => Add(x0, IntL(1))))(0) shouldBe
        "(λx0.x0+1)"

      ShowLambda[Int => Int => Int](Lam(x0 => Lam(x1 => Add(x0, x1))))(0) shouldBe
        "(λx0.(λx1.x0+x1))"

      ShowLambda[Int](App(Lam[ShowH, Int, Int](x0 => Add(x0, IntL(1))), IntL(2)))(0) shouldBe
        "((λx0.x0+1) 2)"
    }
  }
}
