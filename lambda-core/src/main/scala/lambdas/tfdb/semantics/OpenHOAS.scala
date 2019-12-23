package lambdas
package tfdb
package semantics

import tfhoas.{ Lambda => HLambda }, tfdb.{ Lambda => DBLambda }
import OpenHOAS._

// Cf. http://homepages.inf.ed.ac.uk/slindley/papers/unembedding.pdf

abstract class OpenHOAS[Repr[_], E, T] extends (Env[Repr, E] => Repr[T])

object OpenHOAS {

  def apply[Repr[_], E, T](repr: Env[Repr, E] => Repr[T]): OpenHOAS[Repr, E, T] =
    new OpenHOAS[Repr, E, T] {
      def apply(env: Env[Repr, E]): Repr[T] = repr(env)
    }

  sealed abstract class Env[Repr[_], E]
  case class UnitEnv[Repr[_]]()                                    extends Env[Repr, Unit]
  case class ExtendEnv[Repr[_], T, E](h: Repr[T], t: Env[Repr, E]) extends Env[Repr, (T, E)]

  implicit def FromHOAS[Type[_]: ArrowType, Repr[_]](
      implicit H: HLambda[Type, Repr]
  ): DBLambda[Type, OpenHOAS[Repr, ?, ?]] =
    new DBLambda[Type, OpenHOAS[Repr, ?, ?]] {

      def vz[E, T: Type]: OpenHOAS[Repr, (T, E), T] =
        OpenHOAS {
          case ExtendEnv(h, _) => h
        }

      def vs[E, T: Type, T1: Type](a: OpenHOAS[Repr, E, T]): OpenHOAS[Repr, (T1, E), T] =
        OpenHOAS {
          case ExtendEnv(_, t) => a(t)
        }

      def lam[E, T1: Type, T2: Type](t: OpenHOAS[Repr, (T1, E), T2]): OpenHOAS[Repr, E, T1 => T2] =
        OpenHOAS { env =>
          H.lam { t1: Repr[T1] =>
            t(ExtendEnv(t1, env))
          }
        }

      def app[E, T1: Type, T2: Type](
          f: OpenHOAS[Repr, E, T1 => T2]
      )(t1: OpenHOAS[Repr, E, T1]): OpenHOAS[Repr, E, T2] =
        OpenHOAS { env =>
          H.app(f(env))(t1(env))
        }
    }
}
