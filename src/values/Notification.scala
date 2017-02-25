package values

class Notification(val message : String) extends Value {
  override def toString() = message
}

object Notification {
  def apply(msg:String) = new Notification(msg)  
  
  val done = Notification("DONE")
  val ok = Notification("OK")
  val unknown = Notification("UNKNOWN")
  val varUpdate = Notification("VARIABLE UPDATED")
  val bindCreate = Notification("BINDING CREATED")
  val error = Notification("ERROR")
  //etc.
}