name := "StateMonad"

scalaVersion := "2.10.2"

libraryDependencies += "org.specs2" %% "specs2" % "2.2.3" % "test"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases")

scalacOptions in Test ++= Seq("-Yrangepos")

