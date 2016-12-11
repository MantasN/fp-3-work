name := "TicTacToe"

version := "1.0"

scalaVersion := "2.11.8"

//**********************************************************************************
//* Dependencies
//**********************************************************************************
libraryDependencies ++= Seq(
  "io.megam" %% "newman" % "1.3.12",
  "org.scalatest" %% "scalatest" % "2.2.3" % "test"
)