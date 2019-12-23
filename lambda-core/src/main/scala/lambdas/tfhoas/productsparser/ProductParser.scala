package lambdas.tfhoas.productsparser

import lambdas.ProductType
import lambdas.interpreters.{ Interpreter, OpenInterpreter }
import lambdas.safecast.{ Cast, DynTerm }
import lambdas.tfhoas.Products
import lambdas.trees.Tree
import cats.syntax.either._

case class ProductParser[T[_]: ProductType: Cast, F[_]]()(
    implicit
    P: Products[F]
) extends OpenInterpreter[Tree, Either[String, DynTerm[T, F]]]
    with Syntax {

  def apply(rec: => Interpreter[Tree, Either[String, DynTerm[T, F]]]) = {
    case Tuple((a, b)) =>
      for {
        dt1 <- rec(a)
        dt2 <- rec(b)
      } yield
        DynTerm(
          ProductType[T].tProduct(dt1.typ, dt2.typ),
          P.tuple(dt1.term, dt2.term)
        )

    case t =>
      Left(s"\nNot a tuple term $t")
  }
}
