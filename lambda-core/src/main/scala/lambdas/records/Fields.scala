package lambdas
package records

import cats.{ ~>, Applicative, Id }, cats.syntax.functor._, cats.data.Tuple2K
import scala.annotation.implicitNotFound
import shapeless.{ ::, HList, HNil, Witness }, shapeless.labelled._

sealed abstract class Fields[P[_], L <: HList] {
  type A = L
  def apply[Q[_]](nat: P ~> Q): Fields[Q, L]
  def map2[Q[_], R[_]](r: Fields[Q, L])(nat: Tuple2K[P, Q, ?] ~> R): Fields[R, L]
  def sequence(implicit A: Applicative[P]): P[Fields[Id, L]] =
    sequence[P, Id](A, Composed._composed[P, Id])
  def sequence[F[_], G[_]](implicit A: Applicative[F], D: Composed[P, F, G]): F[Fields[G, L]]
  def inverseSeq[Q[_]: Applicative](qfields: Q[Fields[Id, L]]): Fields[Q, L]
}

case class NilFields[P[_]]() extends Fields[P, HNil] {
  override def apply[Q[_]](nat: P ~> Q): Fields[Q, HNil] =
    NilFields[Q]()

  override def map2[Q[_], R[_]](r: Fields[Q, HNil])(nat: Tuple2K[P, Q, ?] ~> R): Fields[R, HNil] =
    NilFields[R]()

  override def sequence[F[_], G[_]](
      implicit A: Applicative[F],
      C: Composed[P, F, G]
  ): F[Fields[G, HNil]] =
    A.pure(NilFields())

  override def inverseSeq[Q[_]: Applicative](qfields: Q[Fields[Id, HNil]]): Fields[Q, HNil] =
    NilFields()
}

// WARNING if a field is added to this case class you need to update equals and hashcode methods
case class HConsFields[P[_], K, V, T <: HList](
    W: Witness.Aux[K],
    head: FieldType[K, P[V]],
    tail: Fields[P, T]
) extends Fields[P, FieldType[K, V] :: T] {

  override def equals(obj: Any): Boolean =
    obj match {
      case other: HConsFields[P, K, V, T] =>
        other.W.value.toString == W.value.toString && head == other.head && tail == other.tail
      case _ => false
    }

  override def hashCode(): Int =
    (head, tail, W.value.toString).hashCode()

  override def apply[Q[_]](nat: P ~> Q): Fields[Q, FieldType[K, V] :: T] =
    HConsFields[Q, K, V, T](W, field[K][Q[V]](nat(head)), tail.apply[Q](nat))

  override def map2[Q[_], R[_]](
      r: Fields[Q, FieldType[K, V] :: T]
  )(nat: Tuple2K[P, Q, ?] ~> R): Fields[R, FieldType[K, V] :: T] = {
    val HConsFields(_, rhead, rtail) = r
    HConsFields[R, K, V, T](
      W,
      field[K][R[V]](nat(Tuple2K[P, Q, V](head, rhead))),
      tail.map2[Q, R](rtail)(nat)
    )
  }

  override def sequence[F[_], G[_]](
      implicit A: Applicative[F],
      composed: Composed[P, F, G]
  ): F[Fields[G, FieldType[K, V] :: T]] =
    A.map2(composed(head), tail.sequence[F, G])(
      (h, t) => HConsFields[G, K, V, T](W, field[K][G[V]](h), t)
    )

  def inverseSeq[Q[_]: Applicative](
      qfields: Q[Fields[Id, FieldType[K, V] :: T]]
  ): Fields[Q, FieldType[K, V] :: T] = {
    val pht: Q[(V, Fields[Id, T])] = qfields.map { case HConsFields(_, h, t) => (h, t) }
    HConsFields[Q, K, V, T](W, field[K][Q[V]](pht.map(_._1)), tail.inverseSeq(pht.map(_._2)))
  }

}

object Fields {

  @implicitNotFound(
    "Implicit not found: Fields.Selector[${P}, ${L}, ${K}]. No field ${K} found in record ${L}"
  )
  abstract class Selector[P[_], L <: HList, K] {
    type Out
    def apply(fields: Fields[P, L]): P[Out]
  }

  object Selector extends LPI {
    type Aux[P[_], L <: HList, K, _Out] = Selector[P, L, K] { type Out = _Out }

    implicit def Found[P[_], K, V, T <: HList] = new Selector[P, FieldType[K, V] :: T, K] {
      type Out = V

      def apply(l: Fields[P, FieldType[K, V] :: T]): P[V] = {
        val HConsFields(_, head, _) = l
        head
      }
    }
  }

  trait LPI {
    implicit def KeepFinding[P[_], K1, V1, T <: HList, K](implicit T: Selector[P, T, K]) =
      new Selector[P, FieldType[K1, V1] :: T, K] {
        type Out = T.Out

        def apply(l: Fields[P, FieldType[K1, V1] :: T]): P[Out] = {
          val HConsFields(_, _, tail) = l
          T(tail)
        }
      }
  }

  def hlist[L <: HList](fields: Fields[Id, L]): L =
    fields match {
      case NilFields()          => HNil
      case HConsFields(_, h, t) => h :: hlist(t)
    }
}
