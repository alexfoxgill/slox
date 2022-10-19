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
    val actual = lox.printed
    assert(actual == expectedOutput)

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

    test("Basic loop") {
      val input =
        """
        var i = 0;
        while (i < 10) {
          i = i + 1;
        }
        print i;
        """

      eval(input, "10")
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

    test("Return") {
      val input =
        """
        fun fib(n) {
          if (n <= 1) return n;
          return fib(n - 2) + fib(n - 1);
        }

        print fib(6);
        """

      eval(input, "8")
    }

    test("Closures") {
      val input =
        """
        fun makeCounter() {
          var i = 0;
          fun count() {
            i = i + 1;
            print i;
          }

          return count;
        }

        var counter = makeCounter();
        counter();
        counter();
        """

      eval(input, "1", "2")
    }

    test("Closure rebinding") {
      val input =
        """
        var a = "global";
        {
          fun showA() {
            print a;
          }

          showA();
          var a = "block";
          showA();
        }
        """

      eval(input, "global", "global")
    }

    test("Basic class") {
      val input =
        """
        class Bagel {}
        print Bagel;
        var bagel = Bagel();
        print bagel;
        """

      eval(input, "Bagel", "Bagel instance")
    }

    test("Class properties") {
      val input =
        """
        class Dog {}
        var basil = Dog();
        basil.name = "basil";
        print basil.name;
        """

      eval(input, "basil")
    }

    test("Class methods") {
      val input =
        """
        class Dog {
          bark() {
            print "Woof!";
          }
        }
        var basil = Dog();
        basil.bark();
        """

      eval(input, "Woof!")
    }

    test("Class this") {
      val input =
        """
        class Dog {
          bark() {
            print this.name + " says Woof!";
          }
        }
        var basil = Dog();
        basil.name = "Basil";
        basil.bark();
        """

      eval(input, "Basil says Woof!")
    }

    test("Class initializer") {
      val input =
        """
        class Dog {
          init(name) {
            this.name = name;
          }
          bark() {
            print this.name + " says Woof!";
          }
        }
        var basil = Dog("Basil");
        basil.bark();
        """

      eval(input, "Basil says Woof!")
    }

    test("Class this binding") {
      val input =
        """
        class Dog {
          init(name) {
            this.name = name;
          }
          bark() {
            print this.name + " says Woof!";
          }
        }
        var basil = Dog("Basil");
        var woof = basil.bark;
        woof();
        """

      eval(input, "Basil says Woof!")
    }

    test("Class inheritance") {
      val input =
        """
        class Animal {
          noise() {
            print this.name + " makes noise";
          }
        }
        class Dog < Animal {
          init(name) {
            this.name = name;
          }
        }
        var basil = Dog("Basil");
        basil.noise();
        """

      eval(input, "Basil makes noise")
    }

    test("Class inheritance super") {
      val input =
        """
        class Animal {
          init(name) {
            this.name = name;
          }
          noise() {
            print this.name + " makes noise";
          }
        }
        class Dog < Animal {
          noise() {
            super.noise();
            print this.name + " barks!";
          }
        }
        var basil = Dog("Basil");
        basil.noise();
        """

      eval(input, "Basil makes noise", "Basil barks!")
    }
  }
