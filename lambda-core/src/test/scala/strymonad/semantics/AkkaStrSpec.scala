package lambdas
package strymonad
package semantics

import arithmetic._

import cats.Id
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import scala.concurrent.Future

import org.scalatest._

class AkkaStreamSpec extends AsyncFlatSpec with Matchers {

  val S = Strymonad[Id, AkkaStreams.S, AkkaStreams.O]; import S._
  val P = new Programs[Id, AkkaStreams.S, AkkaStreams.O]; import P._

  implicit val system       = ActorSystem("QuickStart")
  implicit val materializer = ActorMaterializer()

  "SumSquares" should "work" in {
    val result: Future[Num] = sumSquares(ofArr(Array(1.bd, 2.bd, 3.bd, 4.bd))).run(materializer)
    result map { _ shouldBe 30.bd }
  }
}
