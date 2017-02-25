package expressions

import values._
import ui._

case class Identifier(val name: String) extends Expression {
  def execute(env: Environment): Value = {
    if (env.find(this) == Notification.unknown) { throw new UndefinedException("Undefined identifier : " + name) }
    else 
    env.find(this)
  }
}