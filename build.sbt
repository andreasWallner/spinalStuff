name := "andreasWallner"
version := "0.1"
scalaVersion := "2.11.12"

val spinalVersion = "1.6.0"
val spinalCore = "com.github.spinalhdl" % "spinalhdl-core_2.11" % spinalVersion
val spinalLib = "com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" % "spinalhdl-lib_2.11" % spinalVersion)

libraryDependencies ++= Seq(
  spinalCore, spinalLib, spinalIdslPlugin,
  "org.scalatest" %% "scalatest" % "3.1.1" % "test",
)

logBuffered in Test := false
fork := true
