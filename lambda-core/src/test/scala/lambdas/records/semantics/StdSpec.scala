package lambdas
package records
package semantics

import org.scalatest._
import tfhoas.Lambda
import shapeless.syntax.singleton._
import shapeless._
import shapeless.labelled
import labelled._
import arithmetic._
import lambdas.records.syntax._
import lambdas.records._

class StdSpec extends FunSpec with Matchers {

  val StdReprRecords  = Records[StdRepr]
  val StdReprIntegers = Integers[StdRepr]
  val StdReprLambdas  = Lambda[IntRecordType, StdRepr]
  import StdReprRecords._, StdReprIntegers._, StdReprLambdas._

  case class Age(age: Num)
  case class Person(age: Num, name: Num)

  describe("Records") {

    it("works") {

      Age(1.bd).record[StdRepr] match {
        case RecordStd(GenRecord(HConsFields(_, head, NilFields()))) =>
          head shouldBe int(1)
      }

      // record('age ->> int(1) :: 'name ->> int(2)) match {
      record(Person(1.bd, 2.bd)) match {
        case RecordStd(GenRecord(HConsFields(_, age, HConsFields(_, name, NilFields())))) =>
          age shouldBe int(1)
          name shouldBe int(2)
      }
    }
  }

  describe("Selection") {

    it("works with right fields and case classes") {
      field(record(Age(age = 1)), 'age) shouldBe int(1)
      field(record(Person(age = 1, name = 2)), 'age) shouldBe int(1)
      field(record(Person(age = 1, name = 2)), 'name) shouldBe int(2)
    }

    it("works with right fields") {
      field(record('age ->> int(1)), 'age) shouldBe int(1)
      field(record('age ->> int(1) :: 'name ->> int(2)), 'age) shouldBe int(1)
      field(record('age ->> int(1) :: 'name ->> int(2)), 'name) shouldBe int(2)
      field(record("age" ->> int(1) :: 'name ->> int(2)), "age") shouldBe int(1)
      field(record(1 ->> int(1) :: 2 ->> int(2)), 2) shouldBe int(2)
    }

    it("works with right fields and case class") {
      field(Age(1).record[StdRepr], 'age) shouldBe int(1)
    }

    it("fails with non-existing fields") {
      """field(nilR[StdRepr], 'name)""" shouldNot compile
      """field(record('age ->> int(1)), 'ag)""" shouldNot compile
      """field(record('age ->> int(1)), 'ag)""" shouldNot compile
      """field(record("age" ->> int(1)), "ag")""" shouldNot compile
      """field(record('age ->> int(1) :: 'name ->> int(2)), 'nam)""" shouldNot compile
      """field(record(1 ->> int(1) :: 2 ->> int(2)), 3)""" shouldNot compile
    }

    it("fails with non-existing fields and case classes") {
      """field(Age(1).record[StdRepr], 'ag)""" shouldNot compile
    }

    it("doesn't forget types") {
      field(record(1 ->> int(1.bd) :: 2 ->> int(2)), 2): StdRepr[Num]
      """(field(record(1 ->> int(1) :: 2 ->> int(2)), 2): StdRepr[Boolean])""" shouldNot compile
    }
  }

  describe("Lambdas") {
    it("works") {
      app(lam { i: StdRepr[Num] =>
        field(record('age ->> i), 'age)
      })(int(1)) shouldBe int(1)
    }

    it("works with record args") {
      val wAge = Witness('age)
      wAge.toString // avoids unused warning
      type `{age: Num}` = Record[FieldType[wAge.T, Num] :: HNil]

      lam { r: StdRepr[`{age: Num}`] =>
        field(r, 'age)
      }

      app(lam { r: StdRepr[`{age: Num}`] =>
        field(r, 'age)
      })(Age(1).record[StdRepr])
    }
  }

}
