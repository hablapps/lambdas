package lambdas
package arithmetic

package object syntax {

  implicit class ArithmeticOps[P[_]](p1: P[Num])(implicit A: Arithmetic[P]) {
    def +(p2: P[Num]): P[Num] = A.add(p1)(p2)
    def *(p2: P[Num]): P[Num] = A.mult(p1)(p2)
  }
}
