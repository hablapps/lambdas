package lambdas
package deserialization

sealed abstract class Tree
case class Leaf(l: String) extends Tree
case class Node(r: String, c: List[Tree]) extends Tree

object Tree{

  import scala.util.Try

  object Int{
    def unapply(t: Tree): Option[Int] =
      t match {
        case Node("Int", List(Leaf(i))) =>
          Try(Integer.parseInt(i)).toOption
        case _ =>
          None
      }
  }

  object Add{
    def unapply(t: Tree): Option[(Tree, Tree)] =
      t match {
        case Node("Add", List(e1, e2)) =>
          Some((e1, e2))
        case _ =>
          None
      }
  }

  object Var{
    def unapply(t: Tree): Option[String] =
      t match {
        case Node("Var", List(Leaf(name))) =>
          Some(name)
        case _ =>
          None
      }
  }

  object Lam{
    def unapply(t: Tree): Option[(String, Tree, Tree)] =
      t match {
        case Node("Lam", List(Leaf(name), typ, body)) =>
          Some((name, typ, body))
        case _ =>
          None
      }
  }

  object App{
    def unapply(t: Tree): Option[(Tree, Tree)] =
      t match {
        case Node("App", List(ft, at)) =>
          Some((ft, at))
        case _ =>
          None
      }
  }

  object TArr{
    def unapply(t: Tree): Option[(Tree, Tree)] =
      t match {
        case Node("TArr", List(t1, t2)) =>
          Some((t1, t2))
        case _ =>
          None
      }
  }

  object TInt{
    def unapply(t: Tree): Boolean =
      t match {
        case Leaf("TInt") =>
          true
        case _ =>
          false
      }
  }

  trait Syntax{

    def tr_int(i: Int): Tree =
      Node("Int", List(Leaf(i.toString)))

    def tr_add(i: Tree, j: Tree): Tree =
      Node("Add", List(i, j))

    def tr_vr(name: String): Tree =
      Node("Var", List(Leaf(name)))

    def tr_lam(name: String, typ: Tree, body: Tree): Tree =
      Node("Lam", List(Leaf(name), typ, body))

    def tr_app(f: Tree, a: Tree): Tree =
      Node("App", List(f, a))

    val tr_tInt: Tree =
      Leaf("TInt")

    def tr_tArr(t1: Tree, t2: Tree): Tree =
      Node("TArr", List(t1, t2))
  }
}
