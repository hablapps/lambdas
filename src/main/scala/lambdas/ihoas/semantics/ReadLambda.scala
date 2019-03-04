package lambdas
package initial
package hoas
package semantics

import scala.language.existentials

object ReadLambda{

  val lambdaR = """\(λ(.*?)\.(.*)\)""".r
  val appR = """\((.*) (.*)\)""".r
  val intR = """(\d+)""".r
  val addR = """(.*?)\+(.*?)""".r

  def apply[P[_], T](s: String): Lambda[P, T] =
    applyEnv(s)(Map())

  def applyEnv[P[_], T](s: String)(env: Map[String, Any]): Lambda[P, T] = s match {
    case lambdaR(v, b) =>
      Lam[P, Any, Any]{
        t1 => applyEnv(b)(env + (v -> t1))
      }.asInstanceOf[Lambda[P, T]]
    case appR(f, a) =>
      App[P, Any, T](
        applyEnv(f)(env).asInstanceOf[Lambda[P, Any => T]],
        applyEnv(a)(env).asInstanceOf[Lambda[P, Any]])
    case intR(i) =>
      IntL(Integer.parseInt(i)).asInstanceOf[Lambda[P, T]]
    case addR(i, j) =>
      Add(applyEnv(i)(env), applyEnv(j)(env)).asInstanceOf[Lambda[P, T]]
    case s =>
      env(s).asInstanceOf[Lambda[P, T]]
  }
}
