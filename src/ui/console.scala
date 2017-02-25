package ui

import values._
import expressions._

object console {
  
  val parsers = new JawaParsers
  val globalEnv = new Environment

  def execute(cmmd: String): String = {
    val tree = parsers.parseAll(parsers.expression, cmmd)
    tree match {
      case t: parsers.Failure => throw new SyntaxException(t)
      case _                  => "" + tree.get.execute(globalEnv)
    }
  }

  // etc.
  def repl {
    var more = true
    while (more) {
      try {
        //handle meta commands: quit is done above then do print, execute, save, open, etc. else
        print("-> ")
        val cmmd = readLine();
        val split = cmmd.split("\\s+")
        if (cmmd.equals("quit")) {
          println("Bye") 
          more = false
          return
        }
        else if (split(0).equals("print"))
        {
          for( i <- 1 until split.length) {
            print(split(i) + " ")
          }
          println("")
        }
        else { println(execute(cmmd)) }

      } catch {
        case e: SyntaxException => {
          println(e.gripe)
          println(e.result.msg)
          println("line # = " + e.result.next.pos.line)
          println("column # = " + e.result.next.pos.column)
          println("token = " + e.result.next.first)
        }
        case t: TypeException => {
          println(t.gripe)
        }
        case u: UndefinedException => {
          println(u.gripe)
        }
        case j: JediException => {
          println(j.gripe)
        }
        case any : Exception => {
          println(any)
        }
      } finally {
        // flush input stream
        Console.flush
      }
    }
  } // repl

  def main(args: Array[String]): Unit = { repl } 

} // console
