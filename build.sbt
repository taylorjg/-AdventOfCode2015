val commonCompilerOptions = Seq(
  "-language:postfixOps"
)

val commonDependencies = Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.26"
)

val commonSettings = Seq(
  scalaVersion := "2.12.7",
  scalacOptions ++= commonCompilerOptions,
  libraryDependencies ++= commonDependencies
)

val day1 = project.in(file("Day01")).settings(commonSettings: _*)
val day2 = project.in(file("Day02")).settings(commonSettings: _*)
val day3 = project.in(file("Day03")).settings(commonSettings: _*)
val day4 = project.in(file("Day04")).settings(commonSettings: _*)
val day5 = project.in(file("Day05")).settings(commonSettings: _*)
val day6 = project.in(file("Day06")).settings(commonSettings: _*)
val day7 = project.in(file("Day07")).settings(commonSettings: _*)
val day8 = project.in(file("Day08")).settings(commonSettings: _*)
