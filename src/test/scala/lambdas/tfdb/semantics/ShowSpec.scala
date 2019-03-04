// // package lambdas
// package taglessfinal
// package debruijn
// package semantics

// import org.scalatest._

// class ShowSpec extends FunSpec with Matchers{

//   val E = Examples[ShowB]()
//   implicit val L = Lambda[ShowB]
//   import E._
//   // import L._

//   describe("Show lambda expressions - tagless final - de bruijn"){
//     it("Closed expressions - no lambdas"){
//       (add(int(1), int(3)))(0) shouldBe "(1+3)"
//       ex1[Unit](0) shouldBe "(1+3)"
//       ex1(0) shouldBe "(1+3)"
//     }

//     it("Closed expressions - with lambdas"){
//       (lam(vz))(0) shouldBe "(λx0.x0)"
//       (lam(lam(vs(vz))))(0) shouldBe "(λx0.(λx1.x0))"
//       ex3[Unit](0) shouldBe "(λx0.((x0 1)+2))"
//     }

//     it("Open expressions"){
//       (add(vz, vs(vz)))(0) shouldBe "(y1+y2)"
//       (lam(vs(vz)))(0) shouldBe "(λx0.y1)"
//       (lam(add(vz, vs(vz))))(0) shouldBe "(λx0.(x0+y1))"
//       // ex2[Unit](0) shouldBe "λx0.(x0+x1)"
//       // ex4(0) shouldBe "(x0+x1)"
//     }
//   }
// }
