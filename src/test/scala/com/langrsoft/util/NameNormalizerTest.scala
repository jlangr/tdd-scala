package com.langrsoft.util

import org.scalatest.{BeforeAndAfter, FunSpec, Matchers }

class NameNormalizerTest extends FunSpec with Matchers with BeforeAndAfter {
  describe("a name normalizer") {
    it("returns empty for empty name") {
        NameNormalizer("") shouldBe ""
    }

    it("returns one-word names straight up") {
      NameNormalizer("Madonna") shouldBe "Madonna"
    }

    it("returns last, first for two part names") {
      NameNormalizer("Thomas Hardy") shouldBe "Hardy, Thomas"
    }

    it("removes leading and trailing whitespace") {
      NameNormalizer("  Plato  ") shouldBe "Plato"
    }

    it("removes leading and trailing whitespace from non-mononym") {
      NameNormalizer("  Joe Smith  ") shouldBe "Smith, Joe"
    }

    it("includes middle initial") {
      NameNormalizer("Jeffrey John Langr") shouldBe "Langr, Jeffrey J."
    }

    it("includes multiple middle initials") {
      NameNormalizer("George Raymond Richard Martin") shouldBe "Martin, George R. R."
    }

    it("appends suffixes") {
      NameNormalizer("Wile Evanier Coyote, Esq.") shouldBe "Coyote, Wile E., Esq."  // 1
    }

    // ***extra credit***

    it("appends suffixes to mononym") {
      NameNormalizer("Madonna, Jr.") shouldBe "Madonna, Jr."
    }

    it("throws when name contains too many commas") {
      an [IllegalArgumentException] should be thrownBy NameNormalizer("Wile Evanier Coyote, Esq., Super Genius")
    }
  }
}

// 1: https://web.archive.org/web/20070304081357/http://www.newsfromme.com/archives/2007_02_20.html#012965
