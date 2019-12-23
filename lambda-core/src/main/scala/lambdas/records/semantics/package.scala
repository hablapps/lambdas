package lambdas
package records

import cats.Id
import shapeless.HList

package object semantics {

  type Record[L <: HList] = GenRecord[Id, L]

  implicit class StdRecordOps[L <: HList](r: Record[L]) {
    def hlist: L = Fields.hlist(r.fields)
  }
}
