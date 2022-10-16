ThisBuild/version := "0.1"
ThisBuild/organization := "andreasWallner"
ThisBuild/scalaVersion := "2.11.12"

lazy val spinalHdlIdslPlugin = ProjectRef(file("../SpinalHDL"), "idslplugin")
lazy val spinalHdlSim = ProjectRef(file("../SpinalHDL"), "sim")
lazy val spinalHdlCore = ProjectRef(file("../SpinalHDL"), "core")
lazy val spinalHdlLib = ProjectRef(file("../SpinalHDL"), "lib")

dependsOn(spinalHdlIdslPlugin, spinalHdlSim, spinalHdlCore, spinalHdlLib)
scalacOptions += (artifactPath in (spinalHdlIdslPlugin, Compile, packageBin)).map { file =>
  s"-Xplugin:${file.getAbsolutePath}"
}.value

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.1" % "test",
)

fork := true
