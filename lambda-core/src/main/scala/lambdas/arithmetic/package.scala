package lambdas

package object arithmetic {
  type Num = BigDecimal

  implicit class DoubleOps(v: Double) {
    def bd = BigDecimal(v)
  }
  implicit class IntOps(v: Int) {
    def bd = BigDecimal(v)
  }
  implicit class LongOps(v: Long) {
    def bd = BigDecimal(v)
  }
  implicit class FloatOps(v: Float) {
    def bd = BigDecimal.decimal(v)
  }
}
