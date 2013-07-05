name           := "Dissemination"

version        := "0.2.0-SNAPSHOT"

organization   := "de.sciss"

scalaVersion   := "2.10.2"

description := "Software Component of a Sound Installation"

homepage <<= name { n => Some(url("https://github.com/Sciss/" + n)) }

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies ++= Seq(
  "de.sciss" %% "wolkenpumpe" % "0.35.0"
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

// ---- packaging ----

seq(appbundle.settings: _*)

// appbundle.icon := Some(file("application.icns"))

appbundle.target <<= baseDirectory

