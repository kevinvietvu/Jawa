package expressions

import ui._
import values._

case class Assignment(vbl : Identifier, update : Expression) extends SpecialForm {
    def execute(env: Environment): Value =  
    {
     if (!vbl.execute(env).isInstanceOf[Variable]) throw new TypeException("not a variable type")
     vbl.execute(env).asInstanceOf[Variable].content = update.execute(env)
     return Notification.done 
    }
  
}