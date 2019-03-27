// package lambdas
// package ihoass
// package semantics

// import org.scalatest._

// import trees._
// import TreeSer.{ Show => ShowT }

// class TreeSerSpec extends FunSpec with Matchers {

//   val E = Examples[ShowT](_ => Leaf("anInt"), _ => Leaf("anString"))
//   import E._

//   import tfdbarith.IntArrowType

//   implicit def _IntArrowType = new TreeSerializable[IntArrowType] {
//     def show[A](t: IntArrowType[A]) =
//       Leaf("TInt")
//   }

//   implicit val _IntType = arithmetic.IntType[IntArrowType].tint

//   implicit val i = implicitly[TreeSer[Int, Var[ShowT, Int]]](TreeSer._Var[Int])

//   describe("Serializing initial-hoas encoding") {
//     // TreeSer._Lam[ShowT, Int, Int, Id#λ](_IntArrowType, _IntType, i)(I) shouldBe Node(
//     //   "Lam",
//     //   List(Leaf("x0"), Leaf("TInt"), Node("Var", List(Leaf("x0"))))
//     // )
//   }
// }
