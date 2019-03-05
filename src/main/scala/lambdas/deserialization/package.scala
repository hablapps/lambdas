package lambdas

// http://okmij.org/ftp/tagless-final/course/TypeCheck.hs
// http://okmij.org/ftp/tagless-final/course/Typ.hs

package object deserialization extends Tree.Syntax with TSYM.Syntax{

  type Show[T] = String
}
