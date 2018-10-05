import sbt.Resolver

name := "rings.legs"

version := "1.0"
scalaVersion := "2.12.7"

resolvers += Resolver.mavenLocal

libraryDependencies += "cc.redberry" %% "rings.scaladsl" % "2.5.2"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.25"
libraryDependencies += "junit" % "junit" % "4.12" % Test
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test exclude("junit", "junit-dep")

//mainClass in (Compile, run) := Some("cc.redberry.rings.sym.Main")