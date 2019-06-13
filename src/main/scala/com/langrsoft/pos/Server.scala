package com.langrsoft.pos

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn

object Server {
  def waitOnQuit = {
    println(s"checkout server running at http://localhost:9898/")
    do {
      println("\nEnter 'quit' to stop...")
    } while (StdIn.readLine() != "quit")
  }

  def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem = ActorSystem("checkout-http-server")
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher // needed for future flatmap/oncomplete funcs

    val route:Route = CheckoutRoutesImpl.routes()

    val bindingFuture = Http().bindAndHandle(route,"localhost",9898)

    waitOnQuit

    bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())
  }
}
