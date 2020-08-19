import Versions._

enablePlugins(DockerPlugin)
enablePlugins(JavaAppPackaging)
enablePlugins(AshScriptPlugin)

addCompilerPlugin("org.typelevel" %% "kind-projector" % KindProjector cross CrossVersion.full)
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % BetterMonadicFor)

dockerBaseImage := "openjdk:8-jre-alpine"
dockerExposedPorts ++= Seq(8080)

lazy val commonSettings = Seq(
  name := "banking-api",
  version := "1.0-SNAPSHOT",
  scalaVersion := "2.13.3",
  scalacOptions ++= Seq(
    "-deprecation",
    "-Xfatal-warnings",
    "-Ywarn-value-discard",
    "-Xlint:missing-interpolator"
  )
)

lazy val root = (project in file("."))
  .configs(IntegrationTest)
  .settings(
    commonSettings,
    Defaults.itSettings,
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-server" % Http4s,
      "org.http4s" %% "http4s-circe" % Http4s,
      "org.http4s" %% "http4s-dsl" % Http4s,
      "org.http4s" %% "http4s-blaze-client" % Http4s % "it,test",

      "org.tpolecat" %% "doobie-core" % Doobie,
      "org.tpolecat" %% "doobie-h2" % Doobie,
      "org.tpolecat" %% "doobie-hikari" % Doobie,

      "com.h2database" % "h2" % H2,

      "org.flywaydb" % "flyway-core" % Flyway,

      "io.circe" %% "circe-generic" % Circe,
      "io.circe" %% "circe-literal" % Circe % "it,test",
      "io.circe" %% "circe-optics" % Circe % "it",
      "io.circe" %% "circe-generic-extras" % Circe,
      "io.circe" %% "circe-refined" % Circe,

      "eu.timepit" %% "refined" % Refined,
      "eu.timepit" %% "refined-scalacheck" % Refined % Test,

      "com.github.pureconfig" %% "pureconfig" % PureConfig,
      "com.github.pureconfig" %% "pureconfig-cats-effect" % PureConfig,

      "ch.qos.logback" % "logback-classic" % Logback,
      "io.chrisdavenport" %% "log4cats-core"    % Log4cats,
      "io.chrisdavenport" %% "log4cats-slf4j"   % Log4cats,

      "org.scalatest" %% "scalatest" % ScalaTest % Test,
      "org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.1" % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.1" % Test
    )
  )