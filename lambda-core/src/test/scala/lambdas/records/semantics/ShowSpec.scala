package lambdas
package records
package semantics

import org.scalatest._
import cats.data.Const
import lambdas.arithmetic._
import shapeless.HNil
import shapeless.syntax.singleton._

class ShowSpec extends FunSpec with Matchers {

  import arithmetic.Integers.ShowIntegers._
  import ShowRecords._
  import lambdas.records.syntax._

  describe("Show records") {

    it("works with alternative syntax") {

      nilR[Const[String, ?]].getConst shouldBe
      "{}"

      record("name" ->> int(1)).getConst shouldBe
      "{name=1}"

      record("name" ->> int(1) :: "age" ->> int(2)).getConst shouldBe
      "{name=1, age=2}"

      record('name ->> int(1) :: 'age ->> int(2)).getConst shouldBe
      "{'name=1, 'age=2}"
    }

    implicit val setterRepr = new SetterGen[Const[String, ?], Num, Num] {
      override def apply(): lambdas.Setter[Const[String, Num], Num] =
        new lambdas.Setter[Const[String, Num], Num](
          i => Integers[Const[String, ?]].int(i)
        )
    }

    it("works with shapeless records syntax") {

      (HNil: HNil).record[Const[String, ?]].getConst shouldBe
      "{}"

      ('name ->> 1.bd :: 'age ->> 2.bd :: HNil).record[Const[String, ?]].getConst shouldBe
      "{'name=1, 'age=2}"

      ("name" ->> 1.bd :: "age" ->> 2.bd :: HNil).record[Const[String, ?]].getConst shouldBe
      "{name=1, age=2}"
    }

    it("works with case class records syntax") {

      case class Empty()
      Empty().record[Const[String, ?]].getConst shouldBe
      "{}"

      case class Person(name: Num, age: Num)

      Person(1.bd, 2.bd)
        .record[Const[String, ?]]
        .getConst shouldBe
      "{'name=1, 'age=2}"

    }
  }

}
