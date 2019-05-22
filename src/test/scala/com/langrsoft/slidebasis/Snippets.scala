package com.langrsoft.slidebasis

import org.scalatest.{FunSpec, Matchers}
import org.scalatest.Inspectors._

import scala.util.Random

case class Auto(var rpm: Int = 0) {
  def depressBrake() = {}
  def pressStartButton() = { rpm = 950 + new Random().nextInt(100) }
  def RPM() = { rpm }
}

class Snippets extends FunSpec
  with Matchers {
  describe("something") {
    it("demonstrates some behavior") {
      // Arrange

      // Act

      // Assert
    }
  }

  describe("an auto") {
    val auto = Auto()

    it("demonstrates some behavior") {
      auto.depressBrake()

      auto.pressStartButton()

      auto.RPM() shouldBe 1000 +- 50
    }
  }

  val someCondition = true
  val idleSpeed = 1000
  val alphabetizedName = "Schmoo, Kelly Loo"
  val someList = List('O', 'T', 'T', 'F', 'F', 'S', 'S')
  val someOption: Option[String] = None

  describe("assertions") {
    it("supports lots") {
      someCondition shouldBe true
      someCondition should be (true)
      idleSpeed shouldEqual 1000
      idleSpeed should not equal 2000
      idleSpeed should equal (1000)
      idleSpeed should not be < (900)
      alphabetizedName shouldEqual "Schmoo, Kelly Loo"
      alphabetizedName should startWith regex ("S.*,")
      someList should contain('O')
      someList should contain theSameElementsInOrderAs
        Seq('O', 'T', 'T', 'F', 'F', 'S', 'S')
      someList should contain inOrder('O', 'T', 'S')
      someList should not be empty
      someList should have length 7
      forAll (someList) { c: Char => c.isLetter should be (true)}
      someOption shouldBe empty
    }
  }

}
