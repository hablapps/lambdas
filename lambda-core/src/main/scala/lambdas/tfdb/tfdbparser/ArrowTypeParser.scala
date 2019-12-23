package lambdas
package trees
package tfdbparser

import safecast._
import interpreters._
import cats.syntax.either._

object ArrowTypeParser {

  def apply[T[_]: ArrowType] =
    OpenInterpreter[Tree, String, ATypeTerm[T]] { rec =>
      {
        case TArr(t1, t2) =>
          for {
            t1t <- rec.apply(t1)
            t2t <- rec.apply(t2)
          } yield ATypeTerm(ArrowType[T].tarrow(t1t.typ, t2t.typ))
      }
    }(t => s"Not an Arrow type: $t")
}
