// package lambdas
// package strymonad

// import arithmetic._
// import htfhoas._, HetLam.Syntax._

// class ProgramsHO[Code[_], Str[_], Obs[_], Lam[_]](
//     implicit
//     SM: Strymonad[Code, Str, Obs],
//     A: Arithmetic[Code],
//     LSS: HetLam[Str, Str, Lam],
//     LSO: HetLam[Str, Obs, Lam],
//     LCL: HetLam[Code, Lam, Lam]
// ) {

//   import SM._
//   import A._

//   // First-order programs

//   def squares: Str[Int] => Str[Int] =
//     map[Int, Int](a => mult(a)(a))

//   def sumSquares: Str[Int] => Obs[Int] =
//     str => fold[Int, Int](int(0), add(_)(_))(squares(str))

//   def addCons: Code[Int] => Str[Int] => Str[Int] =
//     const => str => map[Int, Int](add(const)(_))(str)

//   // HO programs

//   def squaresHO: Lam[Str[Int] => Str[Int]] =
//     map[Int, Int](a => mult(a)(a)) _

//   def sumSquaresHO: Lam[Str[Int] => Obs[Int]] =
//     (str: Str[Int]) => fold[Int, Int](int(0), add(_)(_))(squares(str))

//   def addConsHO: Lam[Code[Int] => Lam[Str[Int] => Str[Int]]] =
//     (const: Code[Int], str: Str[Int]) => map[Int, Int](add(const)(_))(str)
// }
