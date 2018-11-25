val commonCompilerOptions = Seq(
  "-language:postfixOps"
)

val commonDependencies = Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.26",
  "org.json4s" %% "json4s-native" % "3.6.2",
  "org.json4s" %% "json4s-jackson" % "3.6.2"
)

val commonSettings = Seq(
  scalaVersion := "2.12.7",
  scalacOptions ++= commonCompilerOptions,
  libraryDependencies ++= commonDependencies
)

val day01 = project.in(file("Day01")).settings(commonSettings: _*)
val day02 = project.in(file("Day02")).settings(commonSettings: _*)
val day03 = project.in(file("Day03")).settings(commonSettings: _*)
val day04 = project.in(file("Day04")).settings(commonSettings: _*)
val day05 = project.in(file("Day05")).settings(commonSettings: _*)
val day06 = project.in(file("Day06")).settings(commonSettings: _*)
val day07 = project.in(file("Day07")).settings(commonSettings: _*)
val day08 = project.in(file("Day08")).settings(commonSettings: _*)
val day09 = project.in(file("Day09")).settings(commonSettings: _*)
val day10 = project.in(file("Day10")).settings(commonSettings: _*)
val day11 = project.in(file("Day11")).settings(commonSettings: _*)
val day12 = project.in(file("Day12")).settings(commonSettings: _*)
val day13 = project.in(file("Day13")).settings(commonSettings: _*)
val day14 = project.in(file("Day14")).settings(commonSettings: _*)
