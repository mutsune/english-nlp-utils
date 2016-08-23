name := "english-nlp-utils"

version := "1.0"

scalaVersion := "2.11.8"

lazy val root = project.in(file(".")).dependsOn(scalaUtilsRepo)

lazy val scalaUtilsRepo = uri("git://github.com/mutsune/scala-utils.git#1.0")

libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-xml" % "1.0.5" withSources() withJavadoc(),
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3" withSources() withJavadoc(),
    "edu.stanford.nlp" % "stanford-corenlp" % "3.5.2" withSources() withJavadoc() artifacts(Artifact("stanford-corenlp", "models"), Artifact("stanford-corenlp")),
    "de.sciss" % "ws4j" % "0.1.0" withSources() withJavadoc()
)
