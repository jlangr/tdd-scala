package com.langrsoft.util

object NameNormalizer {
  def apply(fullName: String) = {
    throwWhenContainsExcessCommas(fullName)
    val name = baseName(fullName.trim())
    if (isMononym(name))
      s"${name}${suffix(fullName)}"
    else
      s"${lastName(name)}, ${firstName(name)}${middleNames(name)}${suffix(fullName)}"
  }

  private def throwWhenContainsExcessCommas(name: String) =
    if (name.count(_ == ',') > 1)
      throw new IllegalArgumentException

  private def suffixSplit(fullName: String) = {
    val nameCommaSuffix = "(.*)\\,(.*)".r
    fullName match {
      case nameCommaSuffix(name, suffix) => (name, s",$suffix")
      case _ => (fullName, "")
    }
  }

  private def baseName(fullName: String) = suffixSplit(fullName)_1

  private def suffix(fullName: String) = suffixSplit(fullName)_2

  private def parts(name: String) = name.split(" ")

  private def lastName(name: String)= {
    val nameParts = parts(name)
    nameParts(nameParts.length - 1)
  }

  private def firstName(name: String) = parts(name)(0)

  private def isMononym(name: String) =
    parts(baseName(name)).length == 1

  private def initial(namePart: String) = s" ${namePart(0)}."

  private def middleNames(name: String) = {
    val nameParts = parts(name)
    nameParts
      .drop(1)
      .take(nameParts.length - 2)
      .map(initial)
      .mkString("")
  }
}
