package lambdas
package trees
package tfdbparser

import tfdb._
import safecast._
import interpreters._

object ArrowTypeParser {

  def apply[T[_]: ArrowType] =
    OpenInterpreter[Tree, String, ATypeTerm[T]] { rec =>
      {
        case TArr(t1, t2) =>
          for {
            t1t <- rec(t1)
            t2t <- rec(t2)
          } yield ATypeTerm(ArrowType[T].tarrow(t1t.typ, t2t.typ))
      }
    }(t => s"Not a type: $t")
}
