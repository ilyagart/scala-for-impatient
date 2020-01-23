package com {
  object inCom {
    val value = "com"
    override def toString: String = s"$value"
  }
  package horstmann {
    object inHorstmann {
      val value = "hostmann"
      override def toString: String = s"$value"
    }
    object Utils {
      def percentOf(value: Double, rate: Double): Double = value * rate / 100
    }
    package impatient {
      object inImpatient {
        val value = "impatient"
        override def toString: String = s"$value"
      }

      import scala.util.Random

      class Employee(var salary: Double) {
        def giveRaise(rate: scala.Double) {
          salary += Utils.percentOf(salary, rate)
        }
      }
      object SomeObj { def someValue: Int = Random.nextInt }
    }
  }
}
