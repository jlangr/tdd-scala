package com.langrsoft.util

object Roman {
  // create a case class for this:.   Could also be case objects
  val conversions = Map(50 -> "L", 10 -> "X", 4 -> "IV", 1 -> "I")

  // Need to learn about case pattern matching;
  // could/should it be used here instead of tuple destructuring?
  def convert(arabic: Integer): String = {
    conversions.foldLeft((arabic, ""))(
      (arabicAndRomanTuple, conversion) => {
      val (arabicDigit, romanDigit) = conversion
      val (arabicRemaining, roman) = arabicAndRomanTuple

      val romanDigitsNeeded = arabicRemaining / arabicDigit
      (arabicRemaining - romanDigitsNeeded * arabicDigit,
       roman + (romanDigit * romanDigitsNeeded))
    })._2
  }
}
