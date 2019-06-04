package com.langrsoft.slidebasis

class BigOldClass {
  val someValue = 123

  def bigOldFunction(): Unit = {
    val x = SomeOtherBigClass().someComplexCalculation(someValue)
    // ...
  }

}

case class SomeOtherBigClass() {
  def someComplexCalculation(someValue: Int) = {
    val result = doStuff(someValue)
    doMoreStuff(result)
    answer
  }

  def answer = 42

  def doMoreStuff(result: Unit) = ???

  def doStuff(someValue: Int) = {}
}

//class WrongPlace {
//   client() {
//      const x = enviousFunction()
//      ...
//   }
//
//   s() {
//      const there = new GreenerGrass()
//      const result = there.doStuff(this.someValue)
//      there.doMoreStuff(result)
//      return there.answer()
//   }
//}
