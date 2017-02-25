package expressions

import values._
import ui._

case class Object(val extension: Identifier, val members: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    var tempEnv = new Environment(env)
    if (extension.name.==("object") || extension.name.==("extends")) {
      for (i <- 0 until members.length ) {
        if (members(i).execute(env).isInstanceOf[Environment])
        {
          tempEnv = new Environment(members(i).execute(env).asInstanceOf[Environment])
        }
        else members(i).execute(tempEnv)       
      }     
    }
    tempEnv
  }
}