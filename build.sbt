ThisBuild/version := "0.1"
ThisBuild/organization := "andreasWallner"
ThisBuild/scalaVersion := "2.13.10"

val spinalVersion = "1.9.4"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion)

libraryDependencies ++= Seq(
  spinalCore, spinalLib, spinalIdslPlugin,
  "org.scalatest" %% "scalatest" % "3.2.15" % "test",
  "com.typesafe.play" %% "play-json" % "2.9.4"
)
scalacOptions += "-language:postfixOps"

fork := true
Test/testForkedParallel := true
