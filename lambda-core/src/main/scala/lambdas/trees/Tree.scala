package lambdas
package trees

import cats.implicits._
import io.circe.{ Decoder, DecodingFailure, Encoder, Json }
import io.circe.syntax._

sealed abstract class Tree
case class Leaf(l: String)                extends Tree
case class Node(r: String, c: List[Tree]) extends Tree

object Tree {
  implicit val CirceTreeEncoder: Encoder[Tree] =
    Encoder.instance {
      case Node(r, trs) => Json.obj(r -> trs.asJson)
      case Leaf(l)      => Json.fromString(l)
    }

  implicit val CirceLeafDecoder: Decoder[Leaf] =
    Decoder[String].map(Leaf)

  implicit val CirceNodeDecoder: Decoder[Node] =
    Decoder.instance { c =>
      val json = c.value
      (for {
        jsonObject <- json.asObject
        h          <- jsonObject.keys.headOption
        v          <- jsonObject.values.headOption
        trees = v.as[List[Tree]]
        node  = trees.map(Node(h, _))
      } yield node).fold(
        DecodingFailure("Expected `object` with `array` value", c.history).asLeft[Node]
      )(identity)
    }

  implicit val CirceTreeDecoder: Decoder[Tree] =
    List[Decoder[Tree]](Decoder[Leaf].widen, Decoder[Node].widen).reduceLeft(_ or _)
}
