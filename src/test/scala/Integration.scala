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
    test("Basic operators") {
      val input =
        """
        var a = 1;
        var b = 2;
        print a+b;
        """

      eval(input, "3")
    }

    test("Block scope") {
      val input =
        """
        var a = 1;
        {
          var a = 2;
          print a;
        }
        print a;"""

      eval(input, "2", "1")
    }

    test("Loops and conditions") {
      val input =
        """
        var acc = "";
        for(var i=0; i<3; i = i + 1) {
          if (i != 1) {
            acc = acc + "a";
          }
        }
        print acc;
        """

      eval(input, "aa")
    }

    test("Functions") {
      val input =
        """
        fun count(n) {
          if (n > 1) count(n - 1);
          print n;
        }

        count(3);
        """

      eval(input, "1", "2", "3")
    }

  }
