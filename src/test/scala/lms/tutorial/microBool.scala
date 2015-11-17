package scala.lms.tutorial

/**
  * Created by boris on 11/16/15.
  * Testing minimal DSLs
  */
import org.scala_lang.virtualized.virtualize

@virtualize
trait MicroBoolIdentity extends MicroBoolDsl {
  def identity(b: Rep[Boolean]): Rep[Boolean] = {
    b
  }
}

@virtualize
class MicroBoolIdentityTest extends TutorialFunSuite {
  val under = "boolID"

  def testIdentity(myBool: Boolean, expected: Boolean) {
    test(s"""identity("$myBool")""") {
      val snippet =
        new MicroBoolDslDriver[Boolean,Boolean] with MicroBoolIdentity {
          def snippet(x: Rep[Boolean]) = identity(x)
        }
      check("", snippet.code)
      assertResult(expected){snippet.eval(myBool)}
    }
  }
  testIdentity(true, true);
  testIdentity(false, false);
}
