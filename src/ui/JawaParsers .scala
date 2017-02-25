package ui

import scala.util.parsing.combinator._

import expressions._
import values._

class JawaParsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | iteration | assignment | disjunction | failure("Invalid expression")

  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^
    {
      case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
    }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^
    {
      case "if" ~ "(" ~ condition ~ ")" ~ consequent ~ None                       => Conditional(condition, consequent)
      case "if" ~ "(" ~ condition ~ ")" ~ consequent ~ Some("else" ~ alternative) => Conditional(condition, consequent, alternative)
    }

  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^
    {
      case con ~ Nil  => con
      case con ~ more => Disjunction(con :: more)
    }

  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^
    {
      case eq ~ Nil  => eq
      case eq ~ more => Conjunction(eq :: more)
    }

  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^
    {
      case eq ~ Nil  => eq
      case eq ~ more => FunCall(Identifier("equals"), eq :: more)
    }

  def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^
    {
      case ineq ~ None           => ineq
      case ineq ~ Some("<" ~ s)  => FunCall(Identifier("less"), List(ineq, s))
      case ineq ~ Some(">" ~ s)  => FunCall(Identifier("more"), List(ineq, s))
      case ineq ~ Some("!=" ~ s) => FunCall(Identifier("unequals"), List(ineq, s))
    }

  def sum: Parser[Expression] =
    product ~ rep(("+" | "-") ~ product ^^ { case "+" ~ s => s case "-" ~ s => negate(s) }) ^^ {
      case prod ~ Nil  => prod
      case prod ~ more => FunCall(Identifier("add"), prod :: more)
    }

  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0)
    FunCall(sub, List(zero, exp))
  }

  def product: Parser[Expression] = term ~ rep(("*" | "/") ~ term ^^ { case "*" ~ s => s case "/" ~ s => invert(s) }) ^^
    {
      case p ~ Nil  => p
      case p ~ more => FunCall(Identifier("mul"), p :: more)
    }

  def invert(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = Number(1)
    FunCall(div, List(one, exp))
  }

  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^
    {
      case None           => Nil
      case Some(e ~ Nil)  => List(e)
      case Some(e ~ exps) => e :: exps
      case _              => Nil
    }

  def lambda: Parser[Expression] = "lambda" ~ parameters ~ expression ^^ {
    case "lambda" ~ para ~ exp => Lambda(para, exp)
  }

  def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^
    {
      case None           => Nil
      case Some(e ~ Nil)  => List(e)
      case Some(e ~ exps) => e :: exps
      case _              => Nil
    }

  def block: Parser[Expression] = opt("object") ~ ("{" ~> expression ~ rep(";" ~> expression) <~ "}") ~ opt("extends" ~ expression) ^^
    {
      case None ~ (exp ~ Nil) ~ None                                => Block(List(exp))
      case None ~ (exp ~ more) ~ None                               => Block(exp :: more)
      case Some("object") ~ (exp ~ Nil) ~ None                      => Object(Identifier("object"), List(exp))
      case Some("object") ~ (exp ~ more) ~ None                     => Object(Identifier("object"), exp :: more)
      case Some("object") ~ (exp ~ Nil) ~ Some("extends" ~ extend)  => Object(Identifier("extends"), List(extend, exp))
      case Some("object") ~ (exp ~ more) ~ Some("extends" ~ extend) => Object(Identifier("extends"), List(extend) ::: exp :: more)
    }

  def funCall: Parser[Expression] = (deref | "(" ~> lambda <~ ")" | access) ~ opt(operands) ^^
    {
      case t ~ None      => t
      case t ~ Some(Nil) => FunCall(t, Nil)
      case t ~ Some(ops) => FunCall(t, ops)
    }

  def iteration: Parser[Iteration] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^
    {
      case "while" ~ "(" ~ cond ~ ")" ~ body => Iteration(cond, body)
    }

  def assignment: Parser[Assignment] = access ~ "=" ~ expression ^^
    {
      case id ~ "=" ~ exp => Assignment(id.asInstanceOf[Identifier], exp)
    }

  def deref: Parser[Expression] = "[" ~> expression <~ "]" ^^
    {
      case e => FunCall(Identifier("content"), List(e))
    }

  def access: Parser[Expression] = identifier ~ rep("." ~> identifier) ^^
    {
      case id ~ Nil  => id
      case id ~ more => Access(id :: more)
    }

  def term: Parser[Expression] = block | lambda | literal | funCall | access | "(" ~> expression <~ ")"

  def literal: Parser[Literal] = number | boole

  def identifier: Parser[Identifier] = """[a-zA-Z][0-9a-zA-Z]*""".r ^^
    {
      case i => Identifier(i)
    }

  def number: Parser[Number] = """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^
    {
      case e => Number(e.toDouble)
    }

  def boole: Parser[Boole] = { "true" ^^ { case x => Boole(true) } | "false" ^^ { case x => Boole(false) } }

}