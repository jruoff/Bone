name := "metric"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.4"
libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.2.4"
libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.11.0"
libraryDependencies += "com.assembla.scala-incubator" %% "graph-dot" % "1.11.0"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.5.4"

scalacOptions ++= Seq("-Xmax-classfile-name","72")

mainClass := Some("Aggregator")
