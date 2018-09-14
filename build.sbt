
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

scalacOptions ++= Seq("-deprecation")

lazy val root = (project in file(".")).
  settings(
    scalaVersion := "2.12.2"
  )
