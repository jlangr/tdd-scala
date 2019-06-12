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

    it("handles mononyms with suffixes") {
      NameNormalizer("Madonna, Jr.") shouldBe "Madonna, Jr."
    }

    it("returns last, first for two part names") {
      NameNormalizer("Thomas Hardy") shouldBe "Hardy, Thomas"
    }

    it("removes leading and trailing whitespace") {
      NameNormalizer("  Plato  ") shouldBe "Plato"
    }

    it("removes leading and trailing whitespace for non-mononym") {
      NameNormalizer("   Mom Langr  ") shouldBe "Langr, Mom"
    }

    it("includes middle initial") {
      NameNormalizer("Jeffrey John Langr") shouldBe "Langr, Jeffrey J."
    }

    it("handles special case where middle name is one letter") {
      NameNormalizer("Harry S Truman") shouldBe "Truman, Harry S"
    }

    it("includes multiple middle initials") {
      NameNormalizer("George Raymond Richard Martin") shouldBe "Martin, George R. R."
    }

    it("appends suffixes") {
      NameNormalizer("Wile Evanier Coyote, Esq.") shouldBe "Coyote, Wile E., Esq."  // 1
    }

    it("throws when name contains too many commas") {
      an [IllegalArgumentException] should be thrownBy NameNormalizer("Wile Evanier Coyote, Esq., Super Genius")
    }

    // Extra Credit:
    //  salutations, e.g. Mr. Edmund Langr. U.S. Salutations are recognized as
    //     one of Mr., Mrs., Ms., and Dr.--either with or without the period.
    //     Anything else should be recognized as a first name

    // What other tests should you write? Not new features but things you know as a programmer
    // that you probably needed? (Or do you need them? Discuss.)
  }

  // TODO Add to exercise Truman
}

// 1: https://web.archive.org/web/20070304081357/http://www.newsfromme.com/archives/2007_02_20.html#012965
