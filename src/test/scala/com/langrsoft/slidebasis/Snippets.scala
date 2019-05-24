package com.langrsoft.slidebasis

import org.joda.time.{DateTime, Days}
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}
import org.scalatest.Inspectors._

import scala.util.Random

case class Auto(var rpm: Int = 0) {
  def depressBrake() = {}
  def pressStartButton() = { rpm = 950 + new Random().nextInt(100) }
  def RPM() = { rpm }
}

class Snippets extends FunSpec
  with Matchers with BeforeAndAfter {
  describe("something") {
    it("demonstrates some behavior") {
      // Arrange

      // Act

      // Assert
    }
  }

  val AgileJava = "QA 123"
  val PatronId = "1"

  case class Material(isbn: String, var daysLate: Int = 0, var dueDate: DateTime = new DateTime(), var available: Boolean = true) {
    def borrow(patronId: String): DateTime = {
      available = false
      dueDate = new DateTime().plusDays(21)
      dueDate
    }
    def returnIt(date: DateTime) = {
      available = true
      daysLate = Days.daysBetween(dueDate.toLocalDate, date.toLocalDate).getDays
    }
    def isAvailable() = { available }

    def fine(): Int = { daysLate * Material.FineAmount }
  }
  object Material {
    val FineAmount: Int = 10
  }

  describe("given a checked-out material") {
    val material = new Material(AgileJava)
    val dueDate = material.borrow(PatronId)

    describe("when returned late") {
      material.returnIt(dueDate.plusDays(2))

      it("is marked available") {
        material.isAvailable shouldBe (true)
      }
      it("generates late fine") {
        material.fine shouldBe (2 * Material.FineAmount)
      }
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

  describe("a recently used list") {
    val recentlyUsedList = RecentlyUsedList()
    it("should sift duplicate filename to top") {
      recentlyUsedList.add("1st")
      recentlyUsedList.add("2nd")
      recentlyUsedList.add("3rd")
      val duplicateOfAddedFirst = "1st"

      recentlyUsedList.add(duplicateOfAddedFirst)

      recentlyUsedList.orderedFilenames shouldEqual
        Seq(duplicateOfAddedFirst, "3rd", "2nd")
    }
  }

  describe("exception") {
    it("throws and logs") {
      try {
        new ErrantClass().doStuffDup()
      }
      catch
      {
        case e: RuntimeException => {}
      }
    }
  }
}

case class RecentlyUsedList(filenames: List[String] = List()) {
  def add(filename: String) = {}
  def orderedFilenames(): List[String] = { List("1st", "3rd", "2nd") }
}

class AppException extends RuntimeException {}

class ErrantClass {
  val System: String = "Finance"
  val Module: String = "PayrollProcessing"

  def trunc(s: String, length: Int)= {
    s.substring(0, length)
  }

  def logError(message: String): Unit = {
//    println(message)
  }

  def doStuff() = {
    try {
      throw new RuntimeException("The system has failed with a critical error. No need to panic just yet. Even though system failure is immininent and will result in the terminate of the host planet, the caches have been purged. All is good.")
    }
    catch {
      case e: RuntimeException => {
        val errMsg = s"${new DateTime()}: ${System}-${Module} HIGH ${trunc(e.getMessage, 80)}"
        logError(errMsg)
        throw new RuntimeException(errMsg, e)
      }
    }
  }
  def doStuffDup() = {
    try {
      throw new RuntimeException("The system has failed with a critical error. No need to panic just yet. Even though system failure is immininent and will result in the terminate of the host planet, the caches have been purged. All is good.")
    }
    catch {
      case e: RuntimeException => {
        logError(format(e, "HIGH"))
        throw new RuntimeException(format(e, "HIGH"), e)
      }
    }
  }

  private def format(e: RuntimeException, severity: String) = {
    s"${new DateTime()}: ${System}-${Module} $severity ${trunc(e.getMessage, 80)}"
  }
}
