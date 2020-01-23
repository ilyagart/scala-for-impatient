// Chapter 8. Inheritance
class Person(val name: String) {
  override def toString = s"${getClass.getName}[name=$name]"
}

class SecretAgent(codename: String) extends Person(codename) {
  override val name = "secret" // Don't want to reveal name ...
  override val toString = "secret" // ... or class name
}

val pleb: Person { def greeting: String } = new Person("random") {
  def greeting = "Greetings, Earthling! My name is Fred."
}
val neo = new SecretAgent("the one")

pleb.name
neo.name

def meet(p: Person { def greeting: String }) {
  println(s"${p.name} says: ${p.greeting}")
}
//object of structural type

meet(pleb)

abstract class Abstraction {
  val id: Int
  var name: String
}

val abstraction = new Abstraction {
  val id: Int = 15
  var name: String = "kek"
}
abstraction.id

def show(o: Any) { println(s"${o.getClass}: $o") }
show(3)
show(3, 4, 5)

