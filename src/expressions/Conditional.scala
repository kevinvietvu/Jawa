package expressions

import values._

case class Conditional(cond: Expression, cons: Expression, val alt: Expression = null) extends SpecialForm {
  def execute(env: Environment): Value = {
    if (cond.execute(env).==(Boole(true))) {  return cons.execute(env) }
    else if (alt != null && !cond.execute(env).isInstanceOf[Number]) { return alt.execute(env) }
    else return Notification.error
  }
}