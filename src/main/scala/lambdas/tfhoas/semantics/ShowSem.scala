package lambdas
package tfhoas
package semantics

object ShowLambda extends Lambda[ShowH]{

  def int(i: Int): ShowH[String] =
    _ => i.toString

  def add(i1: ShowH[String])(i2: ShowH[String]): ShowH[String] =
    c => s"${i1(c)}+${i2(c)}"

  def lam[T1, T2](f: ShowH[String] => ShowH[String]): ShowH[String] =
    c => {
      val x: String = "x" + c
      "(λ" + x + "." + f(_ => x)(c+1) + ")"
    }

  def app[T1, T2](f: ShowH[String])(t1: ShowH[String]): ShowH[String] =
    c => "(" + f(c) + " " + t1(c) + ")"
}
