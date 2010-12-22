import sbt._

class kata(info: ProjectInfo) extends DefaultProject(info) {
  val comonadMaven = "comonad.com Maven Repository" at "http://maven.comonad.com/"

  override def managedStyle = ManagedStyle.Maven
  lazy val publishTo = Resolver.sftp("comonad.com Maven Repository sftp", "maven.comonad.com", "/home/ekmett/comonad.com/maven")

  val layout = "com.comonad" %% "layout" % "0.1"

  // workaround for a buggy pom file
  val kiama = "com.googlecode" %% "kiama" % "1.0.1" from "http://nexus.scala-tools.org/content/groups/hosted/com/googlecode/kiama_2.8.1/1.0.1/kiama_2.8.1-1.0.1.jar"
  val icu = "com.ibm.icu" % "icu4j" % "4.0.1"
}

