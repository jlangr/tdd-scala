package com.langrsoft.util

object NameNormalizer {
  def apply(fullName: String) = {
    throwOnExcessCommas(fullName)

    val name = fullName.trim
    val baseName = removeSuffix(name)

    if (isMononym(baseName))
      name
    else
      s"${last(baseName)}, ${first(baseName)}${middleInits(baseName)}${suffix(name)}"
  }

  def throwOnExcessCommas(name: String) =
    if (name.count(c => c == ',') > 1)
      throw new IllegalArgumentException

  private def suffix(name: String) = {
    val pieces = name.split(",")
    if (pieces.length == 1)
      ""
    else
      s",${pieces(1)}"
  }

  private def removeSuffix(name: String) =
    name.split(",")(0)

  private def suffixSplit(fullName: String) = {
    val nameCommaSuffix = "(.*)\\,(.*)".r
    fullName match {
      case nameCommaSuffix(name, suffix) => (name, s",$suffix")
      case _ => (fullName, "")
    }
  }

  private def parts(name: String) = name.split(" ")

  private def middleInits(name: String) = {
    parts(name)
        .drop(1)
        .take(parts(name).length - 2)
        .map(initial)
        .mkString("")
  }

  private def initial(name: String) = {
    if (name.length == 1)
      s" ${name}"
    else
      s" ${name(0)}."
  }

  private def last(name: String)= {
    val nameParts = parts(name)
    nameParts(nameParts.length - 1)
  }

  private def isMononym(name: String) : Boolean =
    parts(name).length == 1

  private def first(name: String) = parts(name)(0)
}
