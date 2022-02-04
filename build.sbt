lazy val glang = project
  .in(file("."))
  .settings(
    name := "glang",
    version := "current",
    scalaVersion := "3.1.1",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.10"
  )

lazy val createApp = taskKey[Unit]("Create the app.")
createApp := {
  val jars = (Runtime / fullClasspathAsJars).value.map(_.data)
  val appDir = baseDirectory.value / "target" / "app"
  IO.delete(appDir)
//  TODO configure as needed
//  IO.copyDirectory(file("src/main/script"), appDir)
//  (appDir / "dedup.sh").setExecutable(true)
  jars.foreach(file => IO.copyFile(file, appDir / "lib" / file.name))
  streams.value.log.info(s"Built glang app")
}
