import Versions._

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

      "com.github.pureconfig" %% "pureconfig" % PureConfig,
      "com.github.pureconfig" %% "pureconfig-cats-effect" % PureConfig,

      "ch.qos.logback" % "logback-classic" % Logback,

      "org.scalatest" %% "scalatest" % ScalaTest % "it,test",
      "org.scalamock" %% "scalamock" % ScalaMock % "test"
    )
  )