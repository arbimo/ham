// build.sc
import mill._
import scalalib._
import $ivy.`ch.epfl.scala::mill-bloop:1.2.5`
import mill.define.Target
import mill.util.Loose

object deps {
  val cats = ivy"org.typelevel::cats-core:1.4.0"
  val fastparse = ivy"com.lihaoyi::fastparse:2.1.0"
  val ammoniteOps = ivy"com.lihaoyi::ammonite-ops:1.2.1"
  val caseApp = ivy"com.github.alexarchambault::case-app:2.0.0-M3"

  val minitest = ivy"io.monix::minitest:2.3.2"
}
import deps._

trait Module extends ScalaModule {
  def scalaVersion = "2.12.8"
  def scalacOptions = Seq(
    "-Ydelambdafy:inline", "-Ypartial-unification"
    )
}


object core extends Module {
  override def ivyDeps = Agg(cats, fastparse)

  override def compileIvyDeps = Agg(ivy"org.spire-math::kind-projector:0.9.8")

  override def scalacPluginIvyDeps = Agg(ivy"org.spire-math::kind-projector:0.9.8")

  object tests extends Tests { 
    def ivyDeps = Agg(minitest)
    def testFrameworks = Seq("minitest.runner.Framework")
  }

}
