package expressions

import values._
import ui._

case class Access(fields : List[Identifier]) extends SpecialForm {
    def execute(env : Environment) : Value = {
      var tempEnv = env
      for (i <- 0 until fields.length - 1)
      {
        if (!fields(i).execute(tempEnv).isInstanceOf[Environment]) throw new TypeException(fields(i).name + " is not an object")
        tempEnv = fields(i).execute(tempEnv).asInstanceOf[Environment]
      }
      fields(fields.length - 1).execute(tempEnv)
  }
}