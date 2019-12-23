package lambdas.tfhoas.semantics

import lambdas.tfhoas.Products
import lambdas.trees.TreeSerializable.ShowTree
import lambdas.trees.{ Node, Tree }

object ProductsSerializer extends Products[ShowTree] {

  def tuple[A, B](a: ShowTree[A], b: ShowTree[B]): ShowTree[(A, B)] =
    i => tr_tuple(a(i), b(i))

  def fst[A, B](t: ShowTree[(A, B)]): ShowTree[A] =
    i => tr_fst(t(i))

  def snd[A, B](t: ShowTree[(A, B)]): ShowTree[B] =
    i => tr_snd(t(i))

  private def tr_tuple(fst: Tree, snd: Tree): Tree =
    Node("Tuple2", List(fst, snd))

  private def tr_fst(t: Tree): Tree =
    Node("Fst", List(t))

  private def tr_snd(t: Tree): Tree =
    Node("Snd", List(t))
}
