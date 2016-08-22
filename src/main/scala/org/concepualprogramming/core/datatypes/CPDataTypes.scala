package org.concepualprogramming.core.datatypes

/**
 * Created by oleksii.voropai on 8/6/2016.
 */
object CPDataTypes extends Enumeration {
  type CPDataTypes = Value
  val int = Value("int")
  val double = Value("double")
  val string = Value("string")
  val date = Value("date")
  val list = Value("list")
}
