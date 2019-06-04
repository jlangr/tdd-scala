package com.langrsoft.slidebasis

import org.scalatest.Matchers
import scala.concurrent.Future

class FutureTest extends org.scalatest.AsyncFunSpec with Matchers {
  def retrievePage(url: String) = Future {
    scala.io.Source.fromURL(url).mkString
  }

  describe("example.com")  {
    it("contains identifying text") {
      val page = retrievePage("http://example.com")
      page map { page => page should include("Example Domain")}
    }
  }
}

// map assertions onto a Future;
// return the resulting Future[Assertion] to ScalaTest:

