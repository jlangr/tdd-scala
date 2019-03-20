package com.langrsoft.util

class NameNormalizer {
  def normalize(fullName: String) = {
    throwWhenNameContainsExcessCommas(fullName)
    val name = removeSuffix(fullName.trim())
    if (isMononym(name))
      s"${name}${suffix(fullName)}"
    else
      s"${lastName(name)}, ${firstName(name)}${middleNames(name)}${suffix(fullName)}"
  }

  private def throwWhenNameContainsExcessCommas(fullName: String) = {
    if (fullName.count(c => c == ',') > 1)
      throw new IllegalArgumentException
  }

  private def suffixSplit(fullName: String) =
    if (!fullName.contains(","))
      (fullName, "")
    else
    {
      val pattern = "(.*)\\,(.*)".r
      val pattern(mainName, suffix) = fullName
      (mainName, s",$suffix")
    }

  private def removeSuffix(fullName: String): String = {
    val (mainName, _) = suffixSplit(fullName)
    mainName
  }

  private def suffix(fullName: String) = {
    val (_, suffix) = suffixSplit(fullName)
    suffix
  }

  private def parts(name: String) =
    name.split(" ")

  private def lastName(name: String)= {
    val nameParts = parts(name)
    nameParts(nameParts.length - 1)
  }

  private def firstName(name: String) =
    parts(name)(0)

  private def isMononym(name: String) =
    parts(name).length == 1

  private def initial(namePart: String) =
    s" ${namePart(0)}."

  private def middleNames(name: String) = {
    val nameParts = parts(name)
    nameParts
      .drop(1)
      .take(nameParts.length - 2)
      .map(initial)
      .mkString("")
  }
}
