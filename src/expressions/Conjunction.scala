package expressions

import values._
import ui._

case class Conjunction(val operands: List[Expression]) extends SpecialForm {
    def execute(env: Environment): Value = {
    var bool = Boole(true)
    for (operand <- operands) {
      if(operand.execute(env).isInstanceOf[Number]) { throw new TypeException("can't use single integers") }
      if (operand.execute(env).==(Boole(false)) || operand.execute(env).isInstanceOf[Number]) { return Boole(false) }
    }
   bool
  }
}