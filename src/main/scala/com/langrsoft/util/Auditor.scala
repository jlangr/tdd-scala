package com.langrsoft.util

trait Auditor {
  def audit(symbol: String, date: java.util.Date = new java.util.Date())
}
