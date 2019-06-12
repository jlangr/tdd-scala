package com.langrsoft.util

object Roman2 {
  val conversions =
    Seq(
      (1000 -> "M"),
      (900 -> "CM"),
      (500 -> "D"),
      (400 -> "CD"),
      (100 -> "C"),
      (10 -> "X"),
      (4 -> "IV"),
      (1 -> "I"))
  def convert(arabicIncoming: Int) = {
    var arabic = arabicIncoming

    conversions.foldLeft("") { case (s,
                               (arabicDigit, romanDigit)
                              ) => {
      val numberOfRomanDigitsToAppend = arabic / arabicDigit
      arabic -= numberOfRomanDigitsToAppend * arabicDigit
      s + romanDigit * numberOfRomanDigitsToAppend
    }}
  }
}

