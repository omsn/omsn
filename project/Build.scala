import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

  val appName = "omsnblog"
  val appVersion = "1.0"

  val appDependencies = Seq(
    "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
    "com.amazonaws" % "aws-java-sdk" % "1.3.27")

  val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings( // Add your own project settings here      
  )

}
