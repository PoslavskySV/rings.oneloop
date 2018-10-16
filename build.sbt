import sbt.Resolver

name := "rings.legs"

version := "1.0"
scalaVersion := "2.12.7"

resolvers += Resolver.mavenLocal

libraryDependencies += "cc.redberry" %% "rings.scaladsl" % "2.5.3_1"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.25"
libraryDependencies += "junit" % "junit" % "4.12" % Test
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test exclude("junit", "junit-dep")
libraryDependencies += "org.mapdb" % "mapdb" % "3.0.7"
libraryDependencies += "org.rogach" %% "scallop" % "3.1.3"

//mainClass in (Compile, run) := Some("cc.redberry.rings.sym.Main")