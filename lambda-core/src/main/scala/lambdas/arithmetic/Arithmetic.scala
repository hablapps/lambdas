package lambdas
package arithmetic

trait Arithmetic[P[_]] extends Serializable {

  def int(i: Num): P[Num]

  def abs(i: P[Num]): P[Num]

  def mult(i1: P[Num])(i2: P[Num]): P[Num]

  def add(i1: P[Num])(i2: P[Num]): P[Num]

  def max(i1: P[Num])(i2: P[Num]): P[Num]

  def min(i1: P[Num])(i2: P[Num]): P[Num]

  import lambdas.tfhoas._, ArrowType.Implicits._

  def *[T[_]: ArrowType](implicit L: Lambda[T, P], N: T[Num]): P[Num => Num => Num] =
    L.lam { i1 =>
      L.lam { i2 =>
        mult(i1)(i2)
      }
    }

  def +[T[_]: ArrowType](implicit L: Lambda[T, P], N: T[Num]): P[Num => Num => Num] =
    L.lam { i1 =>
      L.lam { i2 =>
        add(i1)(i2)
      }
    }
}

object Arithmetic extends LPI {

  def apply[P[_]](implicit A: Arithmetic[P]) = A

  implicit val ArithmeticId          = semantics.ArithmeticId
  implicit val ArithmeticShow        = semantics.ShowArithFun
  implicit val ArithmeticSerializer  = semantics.ArithmeticSerializer
  implicit val ArithmeticSerializerB = semantics.SerializerB
  implicit val ArithmeticFunction1   = semantics.Function1Arith
  implicit def TupledSem[P1[_, _]: Forall[?[_, _], Arithmetic], P2[_, _]: Forall[
    ?[_, _],
    Arithmetic
  ]] =
    new semantics.TupledInstance[P1, P2]
}

trait LPI {

  implicit def ArithForall[E, P[_, _]](implicit FA: Forall[P, Arithmetic]): Arithmetic[P[E, ?]] =
    FA[E]

  implicit def ForallArithmetic[P[_]](implicit P: Arithmetic[P]) =
    new semantics.ForallOpenHOAS[P]
}
