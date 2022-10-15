import utest._

import scala.collection.mutable.ArrayBuffer

class TestLox extends Lox:
  val printed = ArrayBuffer.empty[String]

  override def print(message: String): Unit =
    printed += message

object IntegrationTests extends TestSuite:

  def eval(code: String, expectedOutput: String*) =
    val lox = new TestLox
    lox.run(code)
    assert(lox.printed == expectedOutput)

  val tests = Tests {
    test("Basic expression") {
      val input =
        """
        var a = 1;
        var b = 2;
        print(a+b);
        """

      eval(input, "3")
    }
  }
