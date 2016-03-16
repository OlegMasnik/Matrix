package khorstman.glava11.task3

import scala.util.Random

object Main extends App {
  val time = System.nanoTime()

  //  val list = (1 to 8000)
  //  list.map { x => x * 20 }
  //  val a = new Fraction(5, 7)
  //  val b = new Fraction(2, 9)
  //  val k = Matrix[Double](4, 5)
  //  println(k)
  //  var c = Matrix(5, 5, Random)
  //  println(c)
  //  c = Matrix(c.data.map { x => x.map { y => y * 10} })
  //  println(c)

  val c = Matrix(2, 2, util.Random)
  println(c)
  val k = Matrix[Double](2, 2, 5)
  println(c ^ 5)

  println((System.nanoTime - time) / 1000000000.0)

  def initMatrix[T](gen: Int, n: Int): Matrix[Double] = gen match {
    case 0 => Matrix(n, n, util.Random)
    case 1 => Matrix[Double](n, n, 1)
  }

  def initVectorString(gen: Int, n: Int): Matrix[Double] = {
    gen match {
      case 0 => Matrix(1, n, util.Random)
      case 1 => Matrix[Double](1, n, 1)
    }
  }

  def initB(n: Int) = {
    Matrix[Double]((for (i <- 1 to n) yield (for (j <- 1 to 1) yield if (i % 2 == 2) 11.0 / (i * i) else i / 11.0).toArray).toArray)
  }
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

class Fraction(val n: Int, val d: Int) {
  private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)
  private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)
  override def toString = num + "/" + den
  def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0
  def gcd(a: Int, b: Int): Int = if (b == 0) math.abs(a) else gcd(b, a % b)
  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

  def +(that: Fraction) = {
    val den = lcm(this.d, that.d)
    new Fraction(den / this.d * this.n + den / that.d * that.n, den)
  }
  def -(that: Fraction) = {
    val den = lcm(this.d, that.d)
    new Fraction(den / this.d * this.n - den / that.d * that.n, den)
  }
  def *(that: Fraction) = new Fraction(this.n * that.n, this.d * that.d)

  def /(that: Fraction) = new Fraction(this.n * that.d, this.d * that.n)

}