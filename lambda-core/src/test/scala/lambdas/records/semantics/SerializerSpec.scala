package lambdas
package records
package semantics

import org.scalatest._
import trees.TreeSerializable.ShowTree
import shapeless.syntax.singleton._

class SerializerSpec extends FunSpec with Matchers {

  import arithmetic.Integers.SerializerIntegers, SerializerIntegers._,
  SerializerIntegers.Constructors._
  import Serializer._, Serializer.Constructors._
  import syntax._

  describe("Serialize records") {

    it("works with alternative syntax") {

      nilR[ShowTree].apply(0) shouldBe
      tr_record(List())

      record("name" ->> int(1)).apply(0) shouldBe
      tr_record(List("name" -> tr_int(1)))

      record("name" ->> int(1) :: "age" ->> int(2)).apply(0) shouldBe
      tr_record(List("name" -> tr_int(1), "age" -> tr_int(2)))

      record('name ->> int(1) :: 'age ->> int(2)).apply(0) shouldBe
      tr_record(List("'name" -> tr_int(1), "'age" -> tr_int(2)))
    }
  }

  describe("Serialize record selection") {

    it("works with alternative syntax") {
      field(record("name" ->> int(1)), "name").apply(0) shouldBe
      tr_field(tr_record(List("name" -> tr_int(1))), "name")
    }
  }
}
