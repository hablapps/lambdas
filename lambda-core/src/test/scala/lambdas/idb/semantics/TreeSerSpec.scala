package lambdas
package idb
package semantics

import org.scalatest._
import TreeSer.Constructors._
import Metamodel._

class TreeSerSpec extends FunSpec with Matchers {
  import safecast.Typeable
  import trees.Treeable
  implicitly[Typeable[Int]]
  implicitly[Treeable[IntStringType]]
  val I = implicitly[TreeSer[Unit, Int => Int, Lam[Unit, Int, Int, Vz[Unit, Int]]]](
    TreeSer._Lam[Unit, Int, Int, Vz[Unit, Int], IntStringType]
  )

  describe("Serializing initial-de bruijn expressions") {
    it("with simple variables") {
      TreeSer[(Int, Unit), Int, Vz[Unit, Int]](
        Vz[Unit, Int]
      ).apply(0) shouldBe tr_vr("y1")

      TreeSer[(String, (Int, Unit)), Int, Vs[(Int, Unit), Int, String, Vz[Unit, Int]]](
        Vs(Vz[Unit, Int])
      ).apply(0) shouldBe tr_vr("y2")
    }

    it("with lambdas") {
      TreeSer[Unit, Int => Int, Lam[Unit, Int, Int, Vz[Unit, Int]]](
        Lam(Vz[Unit, Int])
      )(I).apply(0) shouldBe tr_lam("x0", trees.Leaf("TInt"), tr_vr("x0"))

      // TreeSer[Unit, Int => Int => Int, Lam[
      //   Unit,
      //   Int,
      //   Int => Int,
      //   Lam[(Int, Unit), Int, Int, Vs[(Int, Unit), Int, Int, Vz[Unit, Int]]]
      // ]](
      //   Lam(Lam(Vs(Vz[Unit, Int])))
      // ).apply(0) shouldBe
      // tr_lam("x0", trees.Leaf("unknown"), tr_lam("x1", trees.Leaf("unknown"), tr_vr("x0")))
    }
  }
}
