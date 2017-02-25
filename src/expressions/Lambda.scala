package expressions

import values._
import ui._

case class Lambda(val params: List[Identifier], val body: Expression) extends SpecialForm {
  // (should be defEnv down here) 
  def execute(env: Environment): Value = 
    {
      val closure = new Closure(params,body,env) 
      closure
    }
  
  override def toString() = "Lambda"
}