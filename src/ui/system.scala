package ui

import values._

import expressions._

object system {

  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add"      => add(args)
      case "sub"      => sub(args)
      case "mul"      => mul(args)
      case "div"      => div(args)
      case "equals"   => equals(args)
      case "less"     => less(args)
      case "more"     => more(args)
      case "unequals" => unequals(args)
      case "not"      => not(args)
      case "content"  => content(args)
      case "var"      => makeVar(args)
      case _          => throw new UndefinedException(opcode.name )
    }
  }

  private def add(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ + _)
  }

  private def sub(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("subtraction expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all subtraction inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ - _)
  }

  private def mul(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("multiplication expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all multiplication inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ * _)
  }

  private def div(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("division expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all division inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ / _)
  }

  private def equals(vals: List[Value]): Value = {
    var bool = Boole(false)
    if (vals.length != 2) throw new JediException("equals expects 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    val ok2 = vals.filter(_.isInstanceOf[Boole])
    if (ok.length == vals.length) { bool = (vals(0).asInstanceOf[Number].==(vals(1).asInstanceOf[Number])) }
    else if (ok2.length == vals.length) { bool = (vals(0).asInstanceOf[Boole].==(vals(1).asInstanceOf[Boole])) }
    else { throw new TypeException("both inputs have to be either Number or Boole") }
    bool
  }

  private def unequals(vals: List[Value]): Value = {
    var bool = Boole(false)
    if (vals.length != 2) throw new JediException("equals expects 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    val ok2 = vals.filter(_.isInstanceOf[Boole])
    if (ok.length == vals.length) { bool = (vals(0).asInstanceOf[Number].!=(vals(1).asInstanceOf[Number])) }
    else if (ok2.length == vals.length) { bool = (vals(0).asInstanceOf[Boole].!=(vals(1).asInstanceOf[Boole])) }
    else { throw new TypeException("both inputs have to be either Number or Boole") }
    bool
  }

  private def less(vals: List[Value]): Value = {
    if (vals.length != 2) throw new TypeException("less than expects 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("both inputs must be numbers")
    val bool = (vals(0).asInstanceOf[Number].<(vals(1).asInstanceOf[Number]))
    bool
  }

  private def more(vals: List[Value]): Value = {
    if (vals.length != 2) throw new TypeException("more than expects 2 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("both inputs must be numbers")
    val bool = (vals(0).asInstanceOf[Number].>(vals(1).asInstanceOf[Number]))
    bool
  }

  private def not(vals: List[Value]): Value = {
    if (vals.length != 1) throw new TypeException("not expects one input")
    if (!vals(0).isInstanceOf[Number] && !vals(0).isInstanceOf[Boole]) { throw new UndefinedException(vals(0).toString()) }
    if (vals(0).isInstanceOf[Number]) throw new TypeException("input must be a boole")
    val bool = (vals(0).asInstanceOf[Boole].!)
    bool
  }

  private def content(args: List[Value]): Value = {
    if (!args.head.isInstanceOf[Variable]) throw new TypeException("type must be a variable")
    args.head.asInstanceOf[Variable].content 
  }

  private def makeVar(args: List[Value]): Value = {
    val newVar = new Variable(args.head)
    newVar
  }

}