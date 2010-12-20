import sbt._

class kata(info: ProjectInfo) extends DefaultProject(info) {
  val kiama = "com.googlecode" %% "kiama" % "1.0.1" from "http://scala-tools.org/repo-releases/com/googlecode/kiama_2.8.1/1.0.1/kiama_2.8.1-1.0.1.jar"
}

