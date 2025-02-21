import Dependencies._
import org.scalajs.linker.interface.ModuleSplitStyle

ThisBuild / scalaVersion     := "2.12.18"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.ageofchess"
ThisBuild / organizationName := "com"

enablePlugins(ScalaJSPlugin)

lazy val shared = crossProject(JSPlatform, JVMPlatform).in(file("shared"))
  .settings(
    name := "shared",
    scalaVersion := "2.12.18"
  )
  .jvmSettings()
  .jsSettings(
    // scalaJSUseMainModuleInitializer := true,
     scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("ageofchess")))
     }
  )

lazy val server = project.in(file("server"))
  .dependsOn(shared.jvm)
  .settings(
    name := "server",
    scalaVersion := "2.12.18",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "cask" % "0.9.1",
      "com.lihaoyi" %% "upickle" % "3.1.0"
    )
  )

lazy val client = project.in(file("client"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(shared.js)
  .settings(
    name := "client",
    scalaVersion := "2.12.18",
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("ageofchess")))
     },
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.raquo" %%% "laminar" % "16.0.0"
    )
  )

// lazy val root = (project in file("."))
//   .settings(
//     name := "HelloGitpod",
//     libraryDependencies += scalaTest % Test,
//     scalaJSUseMainModuleInitializer := true,
//      scalaJSLinkerConfig ~= {
//       _.withModuleKind(ModuleKind.ESModule)
//         .withModuleSplitStyle(
//           ModuleSplitStyle.SmallModulesFor(List("ageofchess")))
//     }
//   )

// libraryDependencies ++= Seq(
//   "com.lihaoyi" %% "cask" % "0.9.2",
//   "org.scala-js" %%% "scalajs-dom" % "2.8.0",
//   "com.raquo" %%% "laminar" % "16.0.0"
// )
// Uncomment the following for publishing to Sonatype.
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for more detail.

// ThisBuild / description := "Some descripiton about your project."
// ThisBuild / licenses    := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
// ThisBuild / homepage    := Some(url("https://github.com/example/project"))
// ThisBuild / scmInfo := Some(
//   ScmInfo(
//     url("https://github.com/your-account/your-project"),
//     "scm:git@github.com:your-account/your-project.git"
//   )
// )
// ThisBuild / developers := List(
//   Developer(
//     id    = "Your identifier",
//     name  = "Your Name",
//     email = "your@email",
//     url   = url("http://your.url")
//   )
// )
// ThisBuild / pomIncludeRepository := { _ => false }
// ThisBuild / publishTo := {
//   val nexus = "https://oss.sonatype.org/"
//   if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
//   else Some("releases" at nexus + "service/local/staging/deploy/maven2")
// }
// ThisBuild / publishMavenStyle := true
