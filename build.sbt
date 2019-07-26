name := "multi-validation"

version := "1.3"

scalaVersion := "2.11.12"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.6.0" withSources() withJavadoc(),
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

scalacOptions ++= Seq(
  "-Ypartial-unification"
)