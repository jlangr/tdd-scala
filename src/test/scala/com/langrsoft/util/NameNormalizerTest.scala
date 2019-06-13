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

    ignore("removes leading and trailing whitespace") {
      NameNormalizer("  Plato  ") shouldBe "Plato"
    }

    ignore("includes middle initial") {
      NameNormalizer("Jeffrey John Langr") shouldBe "Langr, Jeffrey J."
    }

    ignore("does not initialize single-letter middle name") {
      NameNormalizer("Harry S Truman") shouldBe "Truman, Harry S"
    }

    ignore("includes multiple middle initials") {
      NameNormalizer("George Raymond Richard Martin") shouldBe "Martin, George R. R."
    }

    ignore("appends suffixes") {
      NameNormalizer("Wile Evanier Coyote, Esq.") shouldBe "Coyote, Wile E., Esq."  // 1
    }

    ignore("throws when name contains too many commas") {
      an [IllegalArgumentException] should be thrownBy NameNormalizer("Wile Evanier Coyote, Esq., Super Genius")
    }

    // Extra Credit:
    //  salutations, e.g. Mr. Edmund Langr. U.S. Salutations are recognized as
    //     one of Mr., Mrs., Ms., and Dr.--either with or without the period.
    //     Anything else should be recognized as a first name

    // What other tests should you write? Not new features but things you know as a programmer
    // that you probably needed? (Or do you need them? Discuss.)
  }
}

// 1: https://web.archive.org/web/20070304081357/http://www.newsfromme.com/archives/2007_02_20.html#012965
