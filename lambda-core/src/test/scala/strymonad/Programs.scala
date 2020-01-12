package lambdas
package strymonad

import arithmetic._

class Programs[Code[_], Str[_], Obs[_]](
    implicit
    SM: Strymonad[Code, Str, Obs],
    A: Arithmetic[Code]
) {

  import SM._
  import A._

  def squares(str: Str[Num]): Str[Num] =
    map[Num, Num](a => mult(a)(a))(str)

  def sumSquares(str: Str[Num]): Obs[Num] =
    runFold[Num, Num](int(0.bd), add(_)(_))(squares(str))

  def addCons(const: Code[Num])(str: Str[Num]): Str[Num] =
    map[Num, Num](add(const)(_))(str)
}
