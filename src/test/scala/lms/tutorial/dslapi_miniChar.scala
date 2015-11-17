package scala.lms.tutorial

import scala.lms.common._
import org.scala_lang.virtualized.virtualize

@virtualize
trait MiniCharMatcherDsl
extends BooleanOps      // ||
with Equal              // infix_==
with PrimitiveOps       // Typ[Char]

@virtualize
trait MiniCharMatcherDslExp extends MiniCharMatcherDsl
with BooleanOpsExp
with EqualExp
with PrimitiveOpsExp

@virtualize
trait MiniCharMatcherDslGen
extends ScalaGenBooleanOps
with ScalaGenEqual
//with ScalaGenPrimitiveOps // not needed, we are not generating any PrimitiveOps

@virtualize
trait MiniCharMatcherDslImpl extends MiniCharMatcherDslExp
{ self =>
  val codegen = new MiniCharMatcherDslGen {
    val IR: self.type = self
  }
}

@virtualize
abstract class MiniCharMatcherDslSnippet[A:Manifest, B:Manifest] extends MiniCharMatcherDsl {
  def snippet(x: Rep[A]): Rep[B]
}

@virtualize
abstract class MiniCharMatcherDslDriver[A:Manifest,B:Manifest] extends MiniCharMatcherDslSnippet[A,B] with MiniCharMatcherDslImpl with CompileScala {
  lazy val f = compile(snippet)(manifestTyp[A],manifestTyp[B])

  def eval(x: A): B = f(x)

  lazy val code: String = {
    val source = new java.io.StringWriter()
    codegen.emitSource(snippet, "Snippet", new java.io.PrintWriter(source))(manifestTyp[A],manifestTyp[B])
    source.toString
  }
}
