package com.langrsoft.util

trait Auditor {
  def audit(symbol: String, date: org.joda.time.DateTime)
}
