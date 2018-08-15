name := "processing-engine"

version := "0.1"

scalaVersion := "2.12.6"

val defaultVersions = Map(
  "chisel3" -> "3.0.+",
  "chisel-iotesters" -> "1.1.+"
)

libraryDependencies ++= Seq("chisel3","chisel-iotesters").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep)) }