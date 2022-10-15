scalaVersion := "3.2.0"

name := "hello-world"
organization := "ch.epfl.scala"
version := "1.0"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.1" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")
