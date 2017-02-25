package values

import expressions._
import ui._

class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {
  def apply(args: List[Value]): Value = {
    val tempEnv = new Environment(defEnv)
    tempEnv.put(params, args)
    body.execute(tempEnv)
  }
  
  override def toString() = "Closure"
}