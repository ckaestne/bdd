scalaVersion := "2.11.7"

libraryDependencies += "de.fosd.typechef" % "featureexprlib_2.11" % "0.4.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"

parallelExecution in Test := false

scalacOptions += "-unchecked"

