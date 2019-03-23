// package lambdas
// package tfdb
// package semantics

// import syntax._
// import org.scalatest._

// import safecast._

// class DynLTermSpec extends Matchers {

//   val d0 = DynLTerm(tint[TypeTerm], int[Function1, Unit](1))

//   def d1[P[_, _]: Lambda]: DynLTerm[P, Unit] =
//     DynLTerm(tint[TypeTerm], 1 + 2)

//   def d2[P[_, _]: Lambda]: DynLTerm[P, Unit] =
//     DynLTerm(tint[TypeTerm] -> tint[TypeTerm], lam(vz[P, Unit, Int] + 1))

//   def d3[P[_, _]: Lambda]: DynLTerm[P, Unit] =
//     // DynLTerm(tint[TypeTerm], lam(add(vz, int(1))).apply(1))
//     DynLTerm(tint[TypeTerm], lam(vz[P, Unit, Int] + 1).apply(1))
// }
