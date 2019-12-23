package lambdas
package records
package parser

import shapeless.{ ::, HNil, Witness }
import shapeless.labelled._
import shapeless.syntax.singleton._
import trees.tfdbparser.DynLTerm
import safecast._
import syntax._
import trees._
import TreeSerializable.{ ShowTree, ShowTreeB }
import tfhoas.Lambda
import arithmetic.{ Integers, Num }
import semantics.Record
import semantics.StdRepr
import semantics.StdRepr
import StdRepr._
import tfdb.semantics.OpenHOAS

import org.scalatest._

class DeserializerSpec extends FunSpec with Matchers with Inside {

  val ArithSerializer  = Integers[ShowTree]
  val RecordSerializer = Records[ShowTree]
  val LambdaSerializer = Lambda[IntRecordType, ShowTree]
  import RecordSerializer._, ArithSerializer._, LambdaSerializer._

  describe("Parsing records") {

    it("just records") {
      val `{age=1}` : Tree = record("age" ->> int(1)).apply(0)

      `{age=1}` shouldBe Node(
        "Record",
        List(Node("Field", List(Leaf("age"), Node("Int", List(Leaf("1"))))))
      )

      val `{age=1, name=2}` : Tree = record("age" ->> int(1) :: "name" ->> int(2)).apply(0)

      inside(IntRecordParser[ShowTreeB].apply(`{age=1}`)(())) {
        case Right(DynLTerm(_, term)) =>
          term(0) shouldBe `{age=1}`
      }

      inside(IntRecordParser[ShowTreeB].apply(`{age=1, name=2}`)(())) {
        case Right(DynLTerm(_, term)) =>
          term(0) shouldBe `{age=1, name=2}`
      }
    }

    it("with right selection") {
      val `{age=1}.age`: Tree =
        field(record("age" ->> int(1)), "age").apply(0)

      val `{age=1, name=2}.age`: Tree =
        field(record("age" ->> int(1) :: "name" ->> int(2)), "age").apply(0)

      val `{age=1, name=2}.name`: Tree =
        field(record("age" ->> int(1) :: "name" ->> int(2)), "name").apply(0)

      inside(IntRecordParser[ShowTreeB].apply(`{age=1}.age`)(())) {
        case Right(DynLTerm(_, term)) =>
          term(0) shouldBe `{age=1}.age`
      }

      inside(IntRecordParser[ShowTreeB].apply(`{age=1, name=2}.age`)(())) {
        case Right(DynTerm(_, term)) =>
          term(0) shouldBe `{age=1, name=2}.age`
      }

      inside(IntRecordParser[ShowTreeB].apply(`{age=1, name=2}.name`)(())) {
        case Right(DynTerm(_, term)) =>
          term(0) shouldBe `{age=1, name=2}.name`
      }
    }

    it("right access - serializer (HO)") {
      val wAge = Witness('age)
      wAge.toString // avoids unused warning
      type `{age: Int}` = Record[FieldType[wAge.T, Num] :: HNil]

      val `_.age`: Tree =
        lam[`{age: Int}`, Num] { r1: ShowTree[`{age: Int}`] =>
          field(r1, 'age)
        }.apply(0)

      inside(IntRecordParser[ShowTreeB].apply(`_.age`)(())) {
        case Right(DynLTerm(_, term)) =>
          term(0) shouldBe `_.age`
      }
    }
  }

  describe("Parsing records made from case classes ") {
    import shapeless.LabelledGeneric

    case class Person(age: Num)
    object Person {
      val L = LabelledGeneric[Person]
      type R = Record[L.Repr]
    }

    it("right access - serializer (HO)") {

      val `_.age`: Tree =
        lam[Person.R, Num] { r1: ShowTree[Person.R] =>
          field(r1, 'age)
        }.apply(0)

      inside(IntRecordParser[ShowTreeB].apply(`_.age`)(())) {
        case Right(DynLTerm(_, term)) =>
          term(0) shouldBe `_.age`
      }
    }
  }

  describe("Parsing wrong records") {
    import semantics.Serializer.Constructors._
    import RecordType.Constructors._
    import tfhoas.semantics.Serializer.Constructors._
    import arithmetic.IntType.Constructors._
    import arithmetic.semantics.ArithmeticSerializer.Constructors._

    it("with wrong selection") {

      val `{}.age1`: Tree =
        tr_field(tr_record(List()), "age1")

      val `{age=1}.age1`: Tree =
        tr_field(tr_record(List("age" -> tr_int(1))), "age1")

      val `{age=1, name=2}.age1`: Tree =
        tr_field(tr_record(List("age" -> tr_int(1), "name" -> tr_int(2))), "age1")

      inside(
        (
          IntRecordParser[ShowTreeB].apply(`{}.age1`)(()),
          IntRecordParser[ShowTreeB].apply(`{age=1}.age1`)(()),
          IntRecordParser[ShowTreeB].apply(`{age=1, name=2}.age1`)(())
        )
      ) {
        case (Left(error1), Left(error2), Left(error3)) =>
          error1.containsSlice("No selector age1 in record type {}") shouldBe true
          error2.containsSlice("No selector age1 in record type {age: TInt}") shouldBe true
          error3.containsSlice("No selector age1 in record type {age: TInt, name: TInt}") shouldBe true
      }
    }

    it("wrong record selection over variable") {

      val `(r: {age: Int} => r.age1)` : Tree =
        tr_lam("r", tr_tRecord(List("age" -> tr_tInt)), tr_field(tr_vr("r"), "age1"))

      `(r: {age: Int} => r.age1)` shouldBe
      Node(
        "Lam",
        List(
          Leaf("r"),
          Node("RecordType", List(Node("FieldType", List(Leaf("age"), Leaf("TInt"))))),
          Node("Select", List(Node("Var", List(Leaf("r"))), Leaf("age1")))
        )
      )

      inside(
        IntRecordParser[OpenHOAS[StdRepr, ?, ?]]
          .apply(`(r: {age: Int} => r.age1)`)(())
      ) {
        case Left(error) => {
          error.containsSlice("No selector age1 in record type {age: TInt}") shouldBe true
        }
      }
    }

    it("wrong record application") {

      val `(_.age)({age1=1})` : Tree =
        tr_app(
          tr_lam("r", tr_tRecord(List("age" -> tr_tInt)), tr_field(tr_vr("r"), "age")),
          tr_record(List(("age1" -> tr_int(1))))
        )

      inside(IntRecordParser[OpenHOAS[StdRepr, ?, ?]].apply(`(_.age)({age1=1})`)(())) {
        case Left(error) => {
          error.containsSlice("Wrong type: {age1: TInt} should be {age: TInt}") shouldBe true
        }
      }
    }
  }

  describe("Standard records") {

    val T = new Samples[ShowTree]
    val S = new Samples[StdRepr]
    val L = Lambda[IntRecordType, StdRepr]
    val I = Integers[StdRepr]
    import S._, L._, I._

    it("lambda on records") {

      inside(IntRecordParser[OpenHOAS[StdRepr, ?, ?]].apply(T.`_.age`(0))(())) {
        case Right(dt) => {
          inside(dt.as1[`{age: Int}` => Num]) {
            case Some(f) =>
              app(f(OpenHOAS.UnitEnv()))(`{age=1}`) shouldBe int(1)
          }
        }
      }
    }

    it("right application of records") {
      inside(IntRecordParser[OpenHOAS[StdRepr, ?, ?]].apply(T.`(_.age)({age=1})`(0))(())) {
        case Right(DynLTerm(_, term)) => {
          term(OpenHOAS.UnitEnv()) shouldBe int(1)
        }
      }
    }
  }
}
