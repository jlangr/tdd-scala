package com.langrsoft.util

object NameNormalizer {
  def apply(name: String) =
    if (isMononym(name))
      name
    else
      s"${lastName(name)}, ${firstName(name)}"

  private def parts(name: String) = name.split(" ")

  private def lastName(name: String)= {
    val nameParts = parts(name)
    nameParts(nameParts.length - 1)
  }

  private def isMononym(name: String) : Boolean =
    parts(name).length == 1

  private def firstName(name: String) = parts(name)(0)
}
