name := "metric"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.7"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.7"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.0"
libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.2.0"
libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.10.1"
libraryDependencies += "com.assembla.scala-incubator" %% "graph-dot" % "1.10.1"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.5.2"

scalacOptions ++= Seq("-Xmax-classfile-name","72")

mainClass := Some("Aggregator")
