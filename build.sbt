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
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
    "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
    "-Xlint:option-implicit", // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals", // Warn if a local definition is unused.
    "-Ywarn-unused:params", // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates", // Warn if a private member is unused.
    "-Ywarn-value-discard", // Warn when non-Unit expression results are unused.
    "-Ybackend-parallelism", "8", // Enable paralellisation — change to desired number!
    "-Ycache-plugin-class-loader:last-modified", // Enables caching of classloaders for compiler plugins
    "-Ycache-macro-class-loader:last-modified", // and macro definitions. This can lead to performance improvements.
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
      "org.tpolecat" %% "doobie-refined" % Doobie,

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
      "io.chrisdavenport" %% "log4cats-core" % Log4cats,
      "io.chrisdavenport" %% "log4cats-slf4j" % Log4cats,

      "org.scalatest" %% "scalatest" % ScalaTest % Test,
      "org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.1" % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.1" % Test
    )
  )