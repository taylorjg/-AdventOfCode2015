val commonSettings = Seq(
  scalaVersion := "2.12.7"
)

val commonDependencies = Seq(
)

val day1 = project.in(file("Day01"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)

val day2 = project.in(file("Day02"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)

val day3 = project.in(file("Day03"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)

val day4 = project.in(file("Day04"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= commonDependencies)
