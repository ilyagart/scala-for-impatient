package ScalaForImpatient21

import scala.io.StdIn

object Chapter21Ex4 extends App {
  // 4
  sealed trait FluentFieldType

  object aString extends FluentFieldType

  object anInt extends FluentFieldType

  object aDouble extends FluentFieldType

  class FluentReader {

    private var data: List[(String, Any)] = List()

    private var nextType: FluentFieldType = aString

    def getData: List[(String, Any)] = data

    def in(fieldType: FluentFieldType): FluentReader = {
      nextType = fieldType
      this
    }

    def and(fieldType: FluentFieldType): FluentReader = in(fieldType)

    def askingFor(fieldName: String): FluentReader = {
      print(fieldName + ": ")

      val value = nextType match {
        case _: aString.type => StdIn.readLine()
        case _: anInt.type   => StdIn.readInt()
        case _: aDouble.type => StdIn.readDouble()
      }

      data = data :+ (fieldName, value)
      this
    }
  }

  def Read: FluentReader = new FluentReader

  Read in aString askingFor "Your name" and anInt askingFor "Your age" and aDouble askingFor "Your weight"
}
