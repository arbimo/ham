import mill._
import scalalib._
import $ivy.`ch.epfl.scala::mill-bloop:1.2.5`
import mill.define.Target
import mill.util.Loose


def gitBasedVersion = T.input {
  os.proc('git, "describe", "--tags").call().out.string.trim()
}


object deps {
  val kindProjector = ivy"org.spire-math::kind-projector:0.9.8"

  val cats = ivy"org.typelevel::cats-core:1.4.0"
  val fastparse = ivy"com.lihaoyi::fastparse:2.1.0"
  val caseApp = ivy"com.github.alexarchambault::case-app:2.0.0-M3"
  val spire = ivy"org.typelevel::spire:0.16.0"

  val minitest = ivy"io.monix::minitest:2.3.2"
}
import deps._

trait HamModule extends ScalaModule {
  def scalaVersion = "2.12.8"
  def scalacOptions = Seq(
    "-Ydelambdafy:inline", "-Ypartial-unification"
    )

  override def compileIvyDeps = Agg(kindProjector)
  override def scalacPluginIvyDeps = Agg(kindProjector)
}


object core extends HamModule {
  override def ivyDeps = Agg(cats, fastparse)

  object tests extends Tests { 
    def ivyDeps = Agg(minitest)
    def testFrameworks = Seq("minitest.runner.Framework")
  }
}

object script extends HamModule {
  override def moduleDeps = Seq( core )
  override def ivyDeps = Agg(cats, fastparse)

  override def mainClass = Some("ham.script.Main")

  object tests extends Tests {
    def ivyDeps = Agg(minitest)
    def testFrameworks = Seq("minitest.runner.Framework")
  }
}

object hydra extends HamModule {
  override def moduleDeps = Seq(core, matrix)
  override def ivyDeps = Agg(cats, fastparse, spire)

  override def mainClass = Some("hydra.Main")

  object tests extends Tests {
    override def mainClass = Some("hydra.EvalTests")
    def ivyDeps = Agg(minitest)
    def testFrameworks = Seq("minitest.runner.Framework")
  }
}

object matrix extends HamModule {

  // csparse module could be a JavaModule but bllop integration requires a scala version to be set
  object csparse extends ScalaModule {
    override def scalaVersion = "2.12.8"

    // tests are not supported in the build
  }

  override def moduleDeps = Seq(csparse)
}

object build extends Module {
  // mill bloop.integrations.mill.Bloop/install
  def bloopInstall(ev: mill.eval.Evaluator) = T.command { bloop.integrations.mill.Bloop.install(ev) }
}