package values

import scala.collection.mutable.HashMap

import ui._
import expressions._

class Environment(var nextEnv: Environment = null) extends HashMap[Identifier, Value] with Value {

  def put(names: List[Identifier], vals: List[Value]) {
    if (names.length != vals.length) throw new SyntaxException()
    for (i <- 0 until names.length) {
      super.put(names(i), vals(i))
    }
  }

  def find(name: Identifier): Value = {

    if (this.contains(name)) {
      return this(name)
    }
    else if (nextEnv != null) {
      nextEnv.find(name)
    } else {
      return Notification.unknown
    }
  }
}

object Environment {
  def main(args: Array[String]): Unit = {
    val id1 = new Identifier("1")
    val id2 = new Identifier("2")
    val id3 = new Identifier("3")

    val val1 = new Number(1)
    val val2 = new Number(2)
    val val3 = new Boole(true)
    val names = List(id1, id2)
    val values = List(val1, val2)

    val globalEnv = new Environment
 
    val localEnv = new Environment(globalEnv)

    localEnv.put(names, values)

    println(localEnv.find(id2))

  }
}