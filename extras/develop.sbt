ThisBuild/version := "0.1"
ThisBuild/organization := "andreasWallner"
ThisBuild/scalaVersion := "2.11.12"

lazy val spinalHdlIdslPlugin = ProjectRef(file("../SpinalHDL"), "idslplugin")
lazy val spinalHdlSim = ProjectRef(file("../SpinalHDL"), "sim")
lazy val spinalHdlCore = ProjectRef(file("../SpinalHDL"), "core")
lazy val spinalHdlLib = ProjectRef(file("../SpinalHDL"), "lib")

lazy val spinalStuff = (project in file(".")).dependsOn(spinalHdlIdslPlugin, spinalHdlSim, spinalHdlCore, spinalHdlLib)
scalacOptions += (spinalHdlIdslPlugin / Compile / packageBin / artifactPath).map { file =>
  s"-Xplugin:${file.getAbsolutePath}"
}.value

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.5" % "test",
)

def stagingClean = Command.command("staging-clean") { currentState =>
  import sys.process._
  import sbt.BuildPaths.{getGlobalBase, getStagingDirectory}

  val stagingDir = getStagingDirectory(currentState, getGlobalBase(currentState))
  ("rm -rf " + stagingDir).!
  currentState
}

fork := true
