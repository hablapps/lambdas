package lambdas
package idb
package semantics

import org.scalatest._
import tfhoas.semantics.Serializer.Constructors._
import arithmetic.IntType.Constructors._
import Metamodel._

class TreeSerSpec extends FunSpec with Matchers {

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
      ).apply(0) shouldBe tr_lam("x0", tr_tInt, tr_vr("x0"))

      TreeSer[Unit, Int => Int => Int, Lam[
        Unit,
        Int,
        Int => Int,
        Lam[(Int, Unit), Int, Int, Vs[(Int, Unit), Int, Int, Vz[Unit, Int]]]
      ]](
        Lam(Lam(Vs(Vz[Unit, Int])))
      ).apply(0) shouldBe
      tr_lam("x0", tr_tInt, tr_lam("x1", tr_tInt, tr_vr("x0")))
    }
  }
}
