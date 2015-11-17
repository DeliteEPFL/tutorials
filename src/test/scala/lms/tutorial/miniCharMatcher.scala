package scala.lms.tutorial

/**
  * Created by boris on 11/16/15.
  * Inspired by the issue occuring with matchchar function in regex.scala
  */
import org.scala_lang.virtualized.virtualize

@virtualize
trait MiniCharMatcher extends MiniCharMatcherDsl {

  def matchChar(c: Char, t: Rep[Char]): Rep[Boolean] = {
//    c == '.' || c == t                      // doesn't work, lift2BooleanOpsCls doesn't kick in
    lift2BooleanOpsCls(c == '.') || c == t  // works

    /**
      * Less important, the essence is in the two above
      */
//    unit(c == '.') || c == t                // works

//    infix_||(unit(c == '.'), c == t)        // works

//    val myB : Boolean = c == '.'
//    val myRB : Rep[Boolean] = c == t
//    lift2BooleanOpsCls(myB) || myRB         // works
  }
}

@virtualize
class MiniCharMatcherTest extends TutorialFunSuite {
  val under = "aCharMatcher"

  def testMatch(charToMatchOn: Char, myChar: Char, expected: Boolean) {
    test(s"""matchChar("$charToMatchOn", "$myChar") == $expected""") {
      val snippet =
        new MiniCharMatcherDslDriver[Char,Boolean] with MiniCharMatcher {
          def snippet(x: Rep[Char]) = matchChar(charToMatchOn, x)
        }
      check("_"+charToMatchOn, snippet.code)
      assertResult(expected){snippet.eval(myChar)}
    }
  }
  testMatch('a', 'a', true);
  testMatch('a', 'b', false);
}
