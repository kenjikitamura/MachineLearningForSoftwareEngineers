lazy val root = (project in file(".")).
  settings(
    name := "Machine Learning",
    version := "1.0",
    scalaVersion := "2.11.4"
  )

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze-viz" % "0.11.2" ,
  "org.apache.commons" % "commons-math3" % "3.3"
)
