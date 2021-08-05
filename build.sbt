scalaVersion := "2.13.3"

name := "stateless"
organization := "kr.ac.korea.prl"
version := "1.0"

libraryDependencies += "org.scalameta" %% "scalameta" % "4.4.18"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.5.1"
libraryDependencies += "org.jgrapht" % "jgrapht-io" % "1.5.1"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"
libraryDependencies += {
  val version = scalaBinaryVersion.value match {
    case "2.10" => "1.0.3"
    case _ ⇒ "2.4.0"
  }
  "com.lihaoyi" % "ammonite" % version % "test" cross CrossVersion.full
}

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue

unmanagedSources / excludeFilter := HiddenFileFilter || "*Notebook*"

// Optional, required for the `source` command to work
(fullClasspath in Test) ++= {
  (updateClassifiers in Test).value
    .configurations
    .find(_.configuration.name == Test.name)
    .get
    .modules
    .flatMap(_.artifacts)
    .collect{case (a, f) if a.classifier == Some("sources") => f}
}
