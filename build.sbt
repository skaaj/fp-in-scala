val commonSettings = Seq(
  scalaVersion := "2.12.10"
)

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )
