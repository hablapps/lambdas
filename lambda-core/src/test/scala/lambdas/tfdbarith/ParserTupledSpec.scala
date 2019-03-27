package lambdas
package tfdbarith

import trees._, tfdbparser._, arithparser._

import org.scalatest._

class TupledParserSpec extends FunSpec with Matchers with Inside {

  describe("Deserializing to several interpreters") {

    it("without variables") {
      inside(
        IntArrowParser[Tupled2[Function1, ShowB]#λ](
          tfdb.Lambda.TupledSem[Function1, ShowB],
          arithmetic.Arithmetic.TupledSem[Function1, ShowB]
        ).apply(tr_app(tr_lam("v0", tr_tInt, tr_vr("v0")), tr_int(1)))(())
      ) {
        case Right(dynTermEvalShow) =>
          dynTermEvalShow.term._2(0) shouldBe "((λx0.x0) 1)"
          dynTermEvalShow.term._1(()) shouldBe 1
      }
    }
  }
}
