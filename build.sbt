ThisBuild/version := "0.1"
ThisBuild/organization := "andreasWallner"
ThisBuild/scalaVersion := "2.11.12"

val spinalVersion = "1.8.0b"
val spinalCore = "com.github.spinalhdl" % "spinalhdl-core_2.11" % spinalVersion
val spinalLib = "com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion)

libraryDependencies ++= Seq(
  spinalCore, spinalLib, spinalIdslPlugin,
  "org.scalatest" %% "scalatest" % "3.1.1" % "test",
)

fork := true
