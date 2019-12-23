package lambdas
package strings

import arithmetic._
import syntax._

class StringsSamples[P[_]: Strings] {

  val s0: P[Num]    = string("hola").length
  val s1: P[String] = "hola"
}
