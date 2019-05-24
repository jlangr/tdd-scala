name := "tdd-scala"

scalaVersion := "2.11.11"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

resolvers ++= Seq(
        "TPS Repository" at "https://nexus.platforms.engineering/repository/tps/",
        Resolver.url("TypeSafe Ivy releases", url("http://dl.bintray.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns),
        "Typesafe" at "https://repo.typesafe.com/typesafe/releases/",
        Resolver.bintrayRepo("hseeberger", "maven"),
        Resolver.url("scoverage-bintray", url("https://dl.bintray.com/sksamuel/sbt-plugins/"))(Resolver.ivyStylePatterns)
)

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

libraryDependencies ++=
    Seq("com.typesafe.akka" %% "akka-slf4j" % "2.4.10",
        "com.typesafe.akka" %% "akka-http-core" % "10.0.9",
        "com.typesafe.akka" %% "akka-http-testkit" % "10.0.9",
        "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.9",
        "de.heikoseeberger" %% "akka-http-play-json" % "1.10.0",
        "com.typesafe.akka" %% "akka-http-xml" % "10.0.9",
        "com.typesafe" % "config" % "1.2.1",
        "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
        "org.scalaj" %% "scalaj-http" % "2.3.0",
        "commons-codec" % "commons-codec" % "1.10",
        "org.mockito" % "mockito-scala_2.11" % "1.4.6" % "test",
        "org.scalatest" % "scalatest_2.11" % "3.0.5" % "test",
        "org.scalactic" %% "scalactic" % "3.0.5"
    )
// "org.mockito" % "mockito-core" % "1.10.17" % "test",
