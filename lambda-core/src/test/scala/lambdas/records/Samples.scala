package lambdas
package records

import semantics.Record
import shapeless.{ ::, HNil, Witness }
import shapeless.labelled._
import shapeless.syntax.singleton._
import arithmetic.{ Integers, Num }
import tfhoas.Lambda
import syntax._

class Samples[Repr[_]](
    implicit
    A: Integers[Repr],
    L: Lambda[IntRecordType, Repr],
    R: Records[Repr]
) {

  // Right programs

  val wAge = Witness("age")
  type `{age: Int}` = Record[FieldType[wAge.T, Num] :: HNil]

  val `{age=1}` : Repr[`{age: Int}`] =
    // R.record(toRecord(toBuilder[Repr, wAge.T, Int](wAge.value ->> A.int(1))))
    R.record(toFields(toBuilder[Repr, wAge.T, Num](field[wAge.T](A.int(1)))))
  // (field[wAge.T](A.int(1)) :: HNil).record[Repr]
  // R.record[`{age: Int}`](field[wAge.T](A.int(1)))

  val wAge1 = Witness("age1")
  type `{age1: Int}` = Record[FieldType[wAge1.T, Num] :: HNil]

  val `{age1=1}` : Repr[`{age1: Int}`] =
    // R.record(toRecord(toBuilder[Repr, wAge.T, Int](wAge.value ->> A.int(1))))
    R.record(toFields(toBuilder[Repr, wAge1.T, Num](field[wAge1.T](A.int(1)))))
  // (field[wAge.T](A.int(1)) :: HNil).record[Repr]
  // R.record[`{age: Int}`](field[wAge.T](A.int(1)))

  val `{age=1}.age`: Repr[Num] =
    R.field(R.record("age" ->> A.int(1)), "age")

  val `_.age`: Repr[`{age: Int}` => Num] =
    L.lam { r1: Repr[`{age: Int}`] =>
      R.field(r1, "age"): Repr[Num]
    }

  val `(_.age)({age=1})` : Repr[Num] =
    L.app(`_.age`)(`{age=1}`): Repr[Num]

  // val `{f1=+}.f1(1, 2)` : Repr[Int] =
  //   R.field1(R.record1("Fun", "f1" -> A.+), "f1")(A.int(1), A.int(2))

  // Wrong programs: do not compile!

  // val `{age=1}.age1`: Repr[Int] =
  //   R.field(`{age=1}`, "age1")

  // val `(_.age)({age1=1})` : Repr[Int] =
  //   L.app(`_.age`)(`{age1=1}`)
}
