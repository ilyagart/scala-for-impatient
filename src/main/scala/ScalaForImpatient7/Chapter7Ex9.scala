package ScalaForImpatient7

object Chapter7Ex9 extends App {
  import java.lang.System._

  val username = getProperty("user.name")
  val password = "secret"
  var buffer = new Array[Byte](password.length)
  in.read(buffer)
  val pass = new String(buffer)
  if (password == pass)
    println(s"Hi $username")
  else err.println("incorrect password")
}
