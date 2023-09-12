scalaVersion := "2.13.11"

resolvers += ("Artifactory OscaR-CETIC" at "http://maven.oscar.cetic.be/artifactory/libs-snapshot-local/").withAllowInsecureProtocol(true)

libraryDependencies ++= Seq(
  "oscar" %% "oscar-cbls" % "5.0.0-SNAPSHOT",
  "com.github.scopt" %% "scopt" % "4.0.0")

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}
