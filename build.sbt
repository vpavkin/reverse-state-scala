lazy val root = (project in file("."))
  .settings(
    organization := "ru.pavkin",
    name := "reverse-state",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.12.6",
    scalacOptions += "-Ypartial-unification",
    libraryDependencies ++= Seq(
      compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
      "org.typelevel" %% "cats-core" % "1.2.0"
    )
  )
  .settings(
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4")
  )
