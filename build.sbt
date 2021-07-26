lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.spinalhdl",
      scalaVersion := "2.11.12",
      version      := "0.1"
    )),
    scalacOptions +=  s"-Xplugin:${new File(baseDirectory.value + "/../SpinalHDL/idslplugin/target/scala-2.11/spinalhdl-idsl-plugin_2.11-1.4.3.jar")}",
    scalacOptions += s"-Xplugin-require:idsl-plugin",
    libraryDependencies ++= Seq(
      //        "com.github.spinalhdl" % "spinalhdl-core_2.11" % "1.4",
      //        "com.github.spinalhdl" % "spinalhdl-lib_2.11" % "1.4",
      "org.scalatest" % "scalatest_2.11" % "3.1.1",
    ),
    name := "andreasWallner"
  ).dependsOn(spinalHdlIdslPlugin, spinalHdlSim,spinalHdlCore,spinalHdlLib)
lazy val spinalHdlIdslPlugin = ProjectRef(file("../SpinalHDL"), "idslplugin")
lazy val spinalHdlSim = ProjectRef(file("../SpinalHDL"), "sim")
lazy val spinalHdlCore = ProjectRef(file("../SpinalHDL"), "core")
lazy val spinalHdlLib = ProjectRef(file("../SpinalHDL"), "lib")

logBuffered in Test := false
fork := true
