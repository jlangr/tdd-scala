package com.langrsoft.slidebasis

class BadBigOldClass {
  val someValue = 123

  def bigOldFunction()= {
    val x = someComplexCalculation()
    // ...
    someLocalFunction(x)
  }

  def someComplexCalculation() = {
    val that = SomeOtherBigClass()
    val result = that.doStuff(this.someValue)
    that.doMoreStuff(result)
    that.answer
  }

  def someLocalFunction(x: Int) = x
}

class BigOldClass {
  val someValue = 123

  def bigOldFunction()= {
    val x = SomeOtherBigClass().someComplexCalculation(someValue)
    // ...
    someLocalFunction(x)
  }

  def someLocalFunction(x: Int) = x
}

case class SomeOtherBigClass() {
  def someComplexCalculation(someValue: Int) = {
    val result = doStuff(someValue)
    doMoreStuff(result)
    answer
  }

  def answer: Int = 42

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
