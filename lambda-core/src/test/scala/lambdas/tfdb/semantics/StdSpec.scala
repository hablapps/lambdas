// // package lambdas
// package taglessfinal
// package debruijn
// package semantics

// import org.scalatest._

// class StdSpec extends FunSpec with Matchers{

//   implicit val L = Lambda[Function1]
//   // import L._

//   describe("Evaluate lambda expressions - tagless final - de Bruijn"){
//     it("Closed expressions - no lambdas"){
//       (add(int(1), int(3)))(()) shouldBe 4
//     }

//     it("Closed expressions - with lambdas"){
//       (lam[Unit, Int, Int](vz))(()) shouldBe "(λx0.x0)"
//       (lam[Unit, Int, Int => Int](lam(vs(vz))))(()) shouldBe "(λx0.(λx1.x0))"
//       // ex3[Unit](0) shouldBe "(λx0.((x0 1)+2))"
//     }

//     it("Open expressions"){
//       (add[(Int, (Int, Unit))](vz, vs(vz)))((3, (5, ()))) shouldBe 8

//       (lam(vs[(Char, Unit), Char, Int](vz)))(('3', ())).apply(0) shouldBe '3'
//       (lam[(Char, Unit), Int, Char](vs(vz)))(('3', ())).apply(0) shouldBe '3'

//       (lam[(Int, Unit), Int, Int](add(vz, vs(vz))))((4, ())).apply(5) shouldBe 9
//     }
//   }
// }
