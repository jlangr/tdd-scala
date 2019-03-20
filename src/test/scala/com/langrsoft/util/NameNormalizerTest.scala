package com.langrsoft.util

import org.scalatest.{BeforeAndAfter, FunSpec, ShouldMatchers }

class NameNormalizerTest extends FunSpec with ShouldMatchers with BeforeAndAfter {
  var normalizer: NameNormalizer = null

  before {
    normalizer = new NameNormalizer
  }

  describe("a name normalizer") {
    it("returns empty for empty name") {
      normalizer.normalize("") shouldBe ""
    }

    it("returns one-word names straight up") {
      normalizer.normalize("Madonna").
        shouldBe("Madonna")
    }

    it("returns last, first for two part names") {
      normalizer.normalize("Thomas Hardy").
        shouldBe("Hardy, Thomas")
    }

    it("removes leading and trailing whitespace") {
      normalizer.normalize("  Plato  ").
        shouldBe("Plato")
    }

    it("removes leading and trailing whitespace from non-mononym") {
      normalizer.normalize("  Joe Smith  ").
        shouldBe("Smith, Joe")
    }

    it("includes middle initial") {
      normalizer.normalize("Jeffrey John Langr").
        shouldBe("Langr, Jeffrey J.")
    }

    it("includes multiple middle initials") {
      normalizer.normalize("George Raymond Richard Martin").
        shouldBe("Martin, George R. R.")
    }

    // ***extra credit***

    ignore("appends suffixes") {
      normalizer.normalize("Wile Evanier Coyote, Esq."). // 1
        shouldBe("Coyote, Wile E., Esq.")
    }

    ignore("appends suffixes to mononym") {
      normalizer.normalize("Madonna, Jr."). // 1
        shouldBe("Madonna, Jr.")
    }

    ignore("throws when name contains too many commas") {
      an [IllegalArgumentException] should be thrownBy
        normalizer.normalize("Wile Evanier Coyote, Esq., Super Genius")
    }
  }
}

// 1: https://web.archive.org/web/20070304081357/http://www.newsfromme.com/archives/2007_02_20.html#012965
