package ScalaForImpatient7

import ScalaForImpatient7.com.horstmann.{Utils, inHorstmann}
import ScalaForImpatient7.com.horstmann.impatient.{SomeObj, inImpatient}
import ScalaForImpatient7.com.inCom

object Chapter7Ex1 extends App {
  println(Utils.percentOf(10, 5))
  println(SomeObj.someValue)
  println(inCom)
  println(inHorstmann)
  println(inImpatient)
}
