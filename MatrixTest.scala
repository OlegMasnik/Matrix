import scala.util.Random

object MatrixTest extends App {

  val c = Matrix(2, 2, util.Random)
  println(c)
  val k = Matrix[Double](2, 2, 5)
  println(c ^ 5)
}

class Matrix[T: Numeric: ClassManifest](val data: Array[Array[T]]) {
  val num = implicitly[Numeric[T]]
  val height = data.length
  val length = data.head.length
  def apply(n: Int, m: Int) = data(n)(m)
  def apply(n: Int, m: Int, elem: T) = data(n)(m) = elem
  def pair(f: (T, T) => T, that: Matrix[T]): Matrix[T] = {
    val temp: Array[Array[T]] = Array.ofDim[T](height, length)
    for (i <- 0 until height; j <- 0 until length) temp(i)(j) = f(data(i)(j), that.data(i)(j))
    new Matrix(temp)
  }
  def +(that: Matrix[T]) = pair(num.plus(_, _), that)
  def -(that: Matrix[T]) = pair(num.minus(_, _), that)
  def ! = new Matrix(this.data.transpose)
  def ^(n: Int): Matrix[T] = if (n == 1) this else pair(num.times(_, _), ^(n - 1))
  def *(that: Matrix[T]) = {
    new Matrix[T](for (x <- this.data) yield for (y <- that.data.transpose) yield x.zip(y) map { x => num.times(x._1, x._2) } sum)
  }
  def *(that: T) = {
    new Matrix[T](this.data.map { x => x map { y => num.times(y, that) } })
  }

  override def toString = data.map(_.mkString("[", ", ", "]")).mkString("\n")
}
object Matrix {
  def apply[T](m: Int, n: Int)(implicit nn: Numeric[T], mm: ClassManifest[T]): Matrix[T] = new Matrix[T](Array.ofDim[T](m, n))
  def apply[T](m: Int, n: Int, v: T)(implicit nn: Numeric[T], mm: ClassManifest[T]): Matrix[T] = new Matrix[T](Array.fill(m, n)(v))
  def apply[T: Numeric: ClassManifest](matrix: Array[Array[T]]) = new Matrix[T](matrix)
  def apply(m: Int, n: Int, r: util.Random): Matrix[Double] = new Matrix(Array.fill[Double](m, n) { Math.floor(r.nextDouble() * 10) + 1 })
}
