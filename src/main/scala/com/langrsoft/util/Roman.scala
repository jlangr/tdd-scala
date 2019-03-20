package com.langrsoft.util

object Roman {
  val digits = List((50, "L"), (10, "X"), (1, "I"))

  // Need to learn about case pattern matching;
  // could/should it be used here instead of tuple destructuring?
  def convert(arabic: Integer): String = {
    digits.foldLeft((arabic, ""))((arabicAndRoman, conversion) => {
      // doesn't look like I can double-destructure the args for this fn
      val (arabicDigit, romanDigit) = conversion
      val (arabicRemaining, roman) = arabicAndRoman
      val romanDigitsNeeded = arabicRemaining / arabicDigit
      (arabicRemaining - romanDigitsNeeded * arabicDigit,
       roman + (romanDigit * romanDigitsNeeded))
    })._2
  }
}
