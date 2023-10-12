ThisBuild/version := "0.1"
ThisBuild/organization := "andreasWallner"
ThisBuild/scalaVersion := "2.13.6"

val spinalVersion = "1.9.3"
val spinalCore = "com.github.spinalhdl" % "spinalhdl-core_2.13" % spinalVersion
val spinalLib = "com.github.spinalhdl" % "spinalhdl-lib_2.13" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" % "spinalhdl-lib_2.13" % spinalVersion)

libraryDependencies ++= Seq(
  spinalCore, spinalLib, spinalIdslPlugin,
  "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  "com.typesafe.play" %% "play-json" % "2.9.4"
)

fork := true
testForkedParallel in Test := true
