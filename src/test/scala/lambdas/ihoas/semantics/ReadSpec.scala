package lambdas
package initial
package hoas
package semantics

import org.scalatest._

class ReadLambdaSpec extends FunSpec with Matchers{

  describe("Read lambda expressions - initial - HOAS"){
    it("works"){

      ReadLambda("1") shouldBe
        IntL(1)

      ReadLambda("1+2") shouldBe
        Add(IntL(1), IntL(2))

      ShowLambda(ReadLambda("(λx0.1+1)"))(0) shouldBe
        "(λx0.1+1)"

      ShowLambda(ReadLambda("(λx0.x0+1)"))(0) shouldBe
        "(λx0.x0+1)"

      ShowLambda(ReadLambda("(λx0.(λx1.x0+x1))"))(0) shouldBe
        "(λx0.(λx1.x0+x1))"

      ShowLambda(ReadLambda("(((λx0.(λx1.x0+x1)) 1) 2)"))(0) shouldBe
        "(((λx0.(λx1.x0+x1)) 1) 2)"
    }

    it("interprets open expressions if the right environment is provided"){

      ReadLambda.applyEnv("x0")(Map("x0" -> Var(1))) shouldBe
        Var(1)

      ReadLambda.applyEnv("x0+1")(Map("x0" -> Var(1))) shouldBe
        Add[λ[? => Int]](Var[λ[T => Int], Int](1), IntL[λ[? => Int]](1))

      import cats.data.Const

      // ReadLambda.applyEnv("x0+1")(Map("x0" -> Var(Const[Int, Int](1)))) shouldBe
      //   Add[Const[Int, ?]](Var[Const[Int, ?], Int](Const(1)), IntL[Const[Int, ?]](1))
    }
  }
}
