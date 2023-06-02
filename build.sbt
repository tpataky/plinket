name := "tiws"

ThisBuild / organization := "com.ihsmarkit.fml"

ThisBuild / scalaVersion := "2.13.8"

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-language:existentials",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-Werror",
  "-Wdead-code",
  "-Wextra-implicit",
  "-Wmacros:after",
  "-Wunused:imports",
  "-Wunused:locals",
  "-Wunused:patvars",
  "-Wunused:privates",
  "-Wvalue-discard",
  "-Wconf:any:warning-verbose",
  "-Xlint:-byname-implicit,_",
  "-Xsource:2.13.0",
  "-Ypatmat-exhaust-depth",
  "off"
)

val catsVersion       = "2.7.0"
val catsEffectVersion = "3.3.5"
val catsParseVersion  = "0.3.6"

libraryDependencies ++= Seq(
  "org.typelevel"     %% "cats-core"   % catsVersion,
  "org.typelevel"     %% "cats-effect" % catsEffectVersion,
  "org.typelevel"     %% "cats-parse"  % catsParseVersion,
  "io.github.tpataky" %% "duckling"    % "0.0.1"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)
