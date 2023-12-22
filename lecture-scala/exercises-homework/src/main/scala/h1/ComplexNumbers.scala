package h1
import scala.language.implicitConversions

class Complex(private val re: Int, private val im: Int):
	def + (that: Complex) : Complex =
		new Complex(re + that.re, im + that.im)

	def - (that: Complex) : Complex =
		new Complex(re - that.re, im - that.im)

	def * (that: Complex) : Complex =
		new Complex(re * that.re - im * that.im, re * that.im + im * that.re)
	def / (that: Complex): Complex =
		(this * that.bar) / that.square

	def / (num: Int) : Complex =
		new Complex(re / num, im / num)

	def unary_- : Complex =
		new Complex(-re, -im)

	def bar : Complex =
		new Complex(re, -im)

	override def toString: String =
		if (im != 0 && re != 0) then
			s"$re${if (im > 0) then "+" else ""}${im}i"
		else if (re != 0) then
			s"${im}i"
		else
			"0"

	private def square: Int =
		re * re + im * im

object Complex:
	def apply(n: Int) =
		new Complex(n, 0)

	def apply(n: Int, m: Int) =
		new Complex(n, m)
		
	given Conversion[Int, Complex] = i => new Complex(i, 0)

object ComplexNumbers:
	private def I: Complex = new Complex(0, 1)
	import Complex.given 

	def main(args: Array[String]): Unit =
		println(Complex(1, 2)) // 1+2i

		println(1 + 2 * I + I * 3 + 2) // 3+5i

		val c = (2 + 3 * I + 1 + 4 * I) * I
		println(-c) // 7-3i