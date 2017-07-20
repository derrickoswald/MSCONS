lazy val root = (project in file(".")).
  settings(
    name := "MSCONSReader",
    version := "2.11-2.0.2-0.0.1",
    scalaVersion := "2.11.8"
  )

libraryDependencies += "org.apache.spark" % "spark-core_2.11" % "2.0.2"
libraryDependencies += "org.apache.spark" % "spark-sql_2.11" % "2.0.2"