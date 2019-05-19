package com.langrsoft.util

object Roman {
  val conversions = Map(50 -> "L", 10 -> "X", 4 -> "IV", 1 -> "I")

  def cvt(arabic: Int, arabicDigit: Int): String = {
    conversions(arabicDigit) + convertR(arabic - arabicDigit)
  }

  def convertR(arabic: Int, s: String =""): String = {
    arabic match {
      case 0 => s
      case n if n >= 50 => cvt(arabic, 50)
      case n if n >= 10 => cvt(arabic, 10)
      case n if n >= 4 => cvt(arabic, 4)
      case _  => cvt(arabic, 1)
    }
  }

  // Need to learn about case pattern matching;
  // could/should it be used here instead of tuple destructuring?
  def convert(arabic: Integer): String = {
    conversions.foldLeft((arabic, ""))(
      (arabicAndRomanTuple, conversion) => {
      // doesn't look like I can double-destructure the args for this fn
      val (arabicDigit, romanDigit) = conversion
      val (arabicRemaining, roman) = arabicAndRomanTuple
      val romanDigitsNeeded = arabicRemaining / arabicDigit
      (arabicRemaining - romanDigitsNeeded * arabicDigit,
       roman + (romanDigit * romanDigitsNeeded))
    })._2
  }
}
