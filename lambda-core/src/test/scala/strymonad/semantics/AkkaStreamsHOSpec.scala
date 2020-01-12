// package lambdas
// package strymonad
// package semantics

// import cats.Id
// import akka.actor.ActorSystem
// import akka.stream.ActorMaterializer
// import akka.stream.scaladsl.Sink
// import scala.concurrent.Future
// import arithmetic._
// import org.scalatest._

// class AkkaStreamHOSpec extends AsyncFlatSpec with Matchers {

//   val S = Strymonad[Id, AkkaStreams.S, AkkaStreams.O]; import S._
//   val P = new ProgramsHO[Id, AkkaStreams.S, AkkaStreams.O, Id]; import P._

//   implicit val system       = ActorSystem("QuickStart")
//   implicit val materializer = ActorMaterializer()

//   "SumSquares" should "work" in {
//     val result: Future[Num] = sumSquares(ofArr(Array(1.bd, 2.bd, 3.bd, 4.bd))).run(materializer)
//     result map { _ shouldBe 30 }
//   }

//   "AddConst" should "work" in {
//     val str: AkkaStreams.S[Num] = addConsHO(1)(ofArr(Array(1.bd, 2.bd, 3.bd, 4.bd)))
//     str.runWith(Sink.seq) map { _ shouldBe List(2.bd, 3.bd, 4.bd, 5.bd) }
//   }
// }
