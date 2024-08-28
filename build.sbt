scalaVersion := "2.13.11"

resolvers += ("Artifactory OscaR-CETIC" at "http://maven.oscar.cetic.be/artifactory/libs-snapshot-local/")
  .withAllowInsecureProtocol(true)

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

libraryDependencies ++= Seq(
  "oscar"            %% "oscar-cbls" % "5.0.0-SNAPSHOT",
  "com.github.scopt" %% "scopt"      % "4.1.0",
  "com.lihaoyi"      %% "upickle"    % "4.0.0"
)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}
