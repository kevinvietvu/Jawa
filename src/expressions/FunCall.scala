package expressions

import values._
import ui._

case class FunCall(val operator: Expression, val operands: List[Expression] = Nil) extends Expression {
  def execute(env: Environment): Value = {
    val args: List[Value] = operands.map(_.execute(env)) //result of evaluating operands
    try {
      val closure = operator.execute(env)
      if (closure.isInstanceOf[Closure]) {
        return closure.asInstanceOf[Closure].apply(args)
      }
      else throw new TypeException("Only functions can be called")
    } catch {
       case _ : Exception =>
        {
            return system.execute(operator.asInstanceOf[Identifier], args)       
        } 
    }
  }
} 

