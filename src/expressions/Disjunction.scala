package expressions

import values._
import ui._


case class Disjunction(val operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    var bool = Boole(false)
    for (operand <- operands) {
      if(operand.execute(env).isInstanceOf[Number]) { throw new TypeException("can't use single integers") }
      if (operand.execute(env).==(Boole(true))) { return Boole(true) }
    }
   bool
  }
}