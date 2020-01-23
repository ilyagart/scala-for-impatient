// Chapter 10. Traits
import java.io.PrintWriter
import scala.language.postfixOps

trait Logger {
  def log(msg: String)
  def info(msg: String) { log(s"INFO: $msg") }
  def warn(msg: String) { log(s"WARN: $msg") }
  def severe(msg: String) { log(s"SEVERE: $msg") }
}

trait ConsoleLogger extends Logger with Cloneable with Serializable {
  def log(msg: String) { println(msg) }
}

trait TimestampLogger extends Logger {
  abstract override def log(msg: String) {
    super.log(s"${java.time.Instant.now()} $msg")
  }
}

trait ShortLogger extends ConsoleLogger {
  val maxLength: Int // A concrete field
  abstract override def log(msg: String) {
    super.log(
      if (msg.length <= maxLength) msg
      else s"${msg.substring(0, maxLength - 3)}..."
    )
  }
}

trait TestLogger extends ConsoleLogger {
  override def log(msg: String): Unit =
    super.log(if (msg.contains("kek")) msg.mkString("[", " ", "]") else msg)
}

class Account {
  var balance = 0.0
}

abstract class SavingsAccount extends Account with ConsoleLogger {
  var interest = 0.0
  val maxLength = 20
  def withdraw(amount: Double) {
    if (amount > balance) log("Insufficient funds")
    else balance -= amount
  }
}

trait FileLogger extends Logger {
  val out = new PrintWriter("app.log") // Part of the trait's constructor
  out.println(s"# ${java.time.Instant.now()}") // Also part of the constructor
  def log(msg: String) { out.println(msg); out.flush() }
}

val acct = new SavingsAccount with ConsoleLogger
val acct1 = new SavingsAccount with TestLogger with ShortLogger
val acct2 = new SavingsAccount with TimestampLogger with TestLogger
val acct3 = new SavingsAccount with ShortLogger with TestLogger
with TimestampLogger
val longAcct = new SavingsAccount with ConsoleLogger with ShortLogger
with TestLogger {
  override val maxLength = 30
}

acct.log("kek")
acct1.log("kek")
acct2.log("kek")
acct2.log("kok")
acct3.log("kek")
acct3.log("kok")
longAcct.log("kek")
longAcct.warn("kek")
longAcct.log("123")

