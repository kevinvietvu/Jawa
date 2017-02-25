package values

import expressions._

case class Boole(val value: Boolean) extends Value with Literal {
  
  def this(value: String) {
    this(value.toBoolean)
  }

  def &&(other: Boole): Boole = Boole(this.value && other.value)

  def ||(other: Boole): Boole = Boole(this.value || other.value)

  def !=(other: Boole): Boole = Boole(this.value != other.value)

  def ==(other: Boole): Boole = Boole(this.value == other.value)
  
  def !() = Boole(!this.value)
  
  override def toString() = "" + value
}

object Boole { 
  
    def test = {
      val b1 = new Boole(true)
      val b2 = new Boole(false)
      val b3 = new Boole("true")
      val b4 = new Boole("false")
      
      println("b1 && b2, Expected : false | Actual : " + b1.&&(b2))
      println("b1 || b2, Expected : true | Actual : " + b1.||(b2))
      println("b1 && b2, Expected : false | Actual : " + b1.&&(b2))
      println("b1 && b3, Expected : true | Actual : " + b1.&&(b3))
      println("b2 && b4, Expected : false | Actual : " + b2.&&(b4))
      println("b1 == b2, Expected : false | Actual : " + b1.==(b2))
      println("b1 != b2, Expected : true | Actual : " + b1.!=(b2))

    }
  
}