package expressions

import values.Notification

import ui._
import values._

case class Iteration(condition: Expression, body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    if (!condition.execute(env).isInstanceOf[Boole]) throw new TypeException("Has to be a boole")
    while (condition.execute(env).asInstanceOf[Boole].value) {
       body.execute(env)
    } 
    return Notification.ok
  }
}