package values

import expressions._
import java.util.function.ToDoubleFunction

case class Number(val value: Double) extends Value with Literal {

  def this(value: String) {
    this(value.toDouble)
  }

  def +(other: Number): Number = Number(this.value + other.value)

  def -(other: Number): Number = Number(this.value - other.value)

  def *(other: Number): Number = Number(this.value * other.value)

  def /(other: Number): Number = Number(this.value / other.value)

  def ==(other: Number): Boole = new Boole(this.value == other.value)

  def <(other: Number): Boole = Boole(this.value < other.value)

  def >(other: Number): Boole = Boole(this.value > other.value)

  def !=(other: Number): Boole = Boole(this.value != other.value)
  
  override def toString() = "" + value
}

object Number {
  def test = {
    val b1 = new Number(3)
    val b2 = new Number(2)
    val b3 = new Number("3")
    val b4 = new Number("2")

    println("b1 + b2, Expected : 5.0 | Actual : " + b1.+(b2))
    println("b1 - b2, Expected : 1.0 | Actual : " + b1.-(b2))
    println("b1 * b2, Expected : 6.0 | Actual : " + b1.*(b2))
    println("b1 / b3, Expected : 1.0 | Actual : " + b1./(b3))
    println("b2 == b4, Expected : true | Actual : " + b2.==(b4))
    println("b1 == b2, Expected : false | Actual : " + b1.==(b2))
    println("b1 != b2, Expected : true | Actual : " + b1.!=(b2))
    println("b1 < b2, Expected : false | Actual : " + b1.<(b2))
    println("b1 > b2, Expected : true | Actual : " + b1.>(b2))
    println("b2 < b1, Expected : true | Actual : " + b2.<(b1))
    println("b2 > b1, Expected : false | Actual : " + b2.>(b1))

  }
}
