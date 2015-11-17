package scala.lms.tutorial

import scala.lms.common._
import org.scala_lang.virtualized.virtualize

@virtualize
trait MicroBoolDsl extends BooleanOps

@virtualize
trait MicroBoolDslExp extends MicroBoolDsl with BooleanOpsExp

@virtualize
trait MicroBoolDslGen extends ScalaGenBooleanOps

@virtualize
trait MicroBoolDslImpl extends MicroBoolDslExp {
  self =>
  val codegen = new MicroBoolDslGen {
    val IR: self.type = self
  }
}

@virtualize
abstract class MicroBoolDslSnippet[A: Manifest, B: Manifest] extends MicroBoolDsl {
  def snippet(x: Rep[A]): Rep[B]
}

@virtualize
abstract class MicroBoolDslDriver[A: Manifest, B: Manifest] extends MicroBoolDslSnippet[A, B] with MicroBoolDslImpl with CompileScala {
  lazy val f = compile(snippet)(manifestTyp[A], manifestTyp[B])

  def eval(x: A): B = f(x)

  lazy val code: String = {
    val source = new java.io.StringWriter()
    codegen.emitSource(snippet, "Snippet", new java.io.PrintWriter(source))(manifestTyp[A], manifestTyp[B])
    source.toString
  }
}
