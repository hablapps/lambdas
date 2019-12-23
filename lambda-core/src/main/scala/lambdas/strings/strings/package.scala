package lambdas
package strings

import arithmetic._

package object syntax {

  implicit def string[P[_]](s: String)(implicit S: Strings[P]): P[String] =
    S.string(s)

  implicit class StringsOps[P[_]](p: P[String])(implicit S: Strings[P]) {
    def length: P[Num] =
      S.length(p)
  }
}
