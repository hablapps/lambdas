package lambdas
package trees

sealed abstract class Tree
case class Leaf(l: String)                extends Tree
case class Node(r: String, c: List[Tree]) extends Tree
