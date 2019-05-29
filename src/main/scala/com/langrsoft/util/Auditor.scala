package com.langrsoft.util

import org.joda.time.DateTime

trait Auditor {
  def audit(symbol: String, timestamp: DateTime)
}
