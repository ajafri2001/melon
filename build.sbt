scalaVersion := "3.7.2"

Global / semanticdbEnabled := true

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Wunused:all"
)

libraryDependencies += "org.scalameta" %% "munit" % "1.2.0" % Test
libraryDependencies += "io.github.classgraph" % "classgraph" % "4.8.184"
