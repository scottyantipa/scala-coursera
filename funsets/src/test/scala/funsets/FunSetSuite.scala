package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s1_4 = (x: Int) => (x > 0) && (x < 5)
    val s2_6 = (x: Int) => (x > 1) && (x < 7)
    val sneg2_2 = (x: Int) => (x > -3) && (x < 3)
    val all_int = (x: Int) => true
    val empty = (x: Int) => false
    val all_pos = (x: Int) => x > 0
    val all_neg = (x: Int) => x < 0
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains elements that are in each set") {
    new TestSets {
      assert(intersect(s1, s2)(3) === false, "Intersection of singletons")
      assert(intersect(s1_4, s2_6)(4), "Intersection of range")
    }
  }

  test("diff contains elements in s that are not in t") {
    new TestSets {
      assert(diff(s1, s2)(1), "Diff 1 against s1 and s2")
      assert(diff(s1_4, s2_6)(1) === true, "Diff range")
    }
  }

  test("filter a set" ) {
    new TestSets {
      assert( filter(all_int, empty)(1) === false, "False predicate" )
      assert( filter(sneg2_2, all_pos)(1) === true, "Simple filter" )
    }
  }

  test("forall") {
    new TestSets {
      assert( forall(s1, s2) === false, "forall of two singletons" )
      assert( forall(all_pos, (x: Int) => x > -1) === true, "All pos are greater than -1" )
      assert( forall(all_int, all_pos) === false, "Not all ints are positive" )
    }
  }

  test("exists") {
    new TestSets {
      assert( exists(s1, s1) === true, "singleton exists in self")
      assert( exists(sneg2_2, s1) === true, "1 exists in -2 to 2")
      assert( exists(all_pos, all_neg) === false, "no negatives in all_pos")
    }
  }

  test("map") {
    new TestSets {
      assert( contains(map(s1, (x: Int) => x + 1), 2) === true, "s1 maps to s2" )

      val posMappedToNeg = map(all_pos, (x: Int) => -x)
      assert( forall(posMappedToNeg, all_neg) === true, "map pos to neg" )
    }
  }

}
