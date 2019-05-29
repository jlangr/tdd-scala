package com.langrsoft.util

class FSAuditor extends Auditor {
    def audit(symbol: String, timestamp: org.joda.time.DateTime) =
        throw new RuntimeException("system unavailable")
}
