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
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
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

  test("singletonSet(2) contains 2") {
    new TestSets {
      assert(contains(s2, 2), "Singleton")
    }
  }

  test("singletonSet(3) contains 3") {
    new TestSets {
      assert(contains(s3, 3), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains only elements in both sets") {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s2, s3)
      val i = intersect(s, t)
      assert(!contains(i, 1), "Intersection i-1")
      assert(contains(i, 2), "Intersection i-2")
      assert(!contains(i, 3), "Intersection i-3")

      val j = intersect(s1, s2)
      assert(!contains(j, 1), "Intersection j-1")
      assert(!contains(j, 2), "Intersection j-2")
      assert(!contains(j, 3), "Intersection j-3")
    }
  }

  test("difference contains only elements in 1st set which aren't in 2nd set") {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s, s3)
      val d = diff(s, s2)
      assert(contains(d, 1), "Difference {1,2} / {2} should contain 1")
      assert(!contains(d, 2), "Difference {1,2} / {2} should NOT contain 2")
      assert(!contains(d, 3), "Difference {1,2} / {2} should NOT contain 3")

      val j = diff(t, s)
      assert(!contains(j, 1), "Difference {1,2,3} / {1,2} should NOT contain 1")
      assert(!contains(j, 2), "Difference {1,2,3} / {1,2} should NOT contain 2")
      assert(contains(j, 3), "Difference {1,2,3} / {1,2} should contain 3")
    }
  }

  test("filter selects only the elements of a set that are accepted by the provided predicate") {
    val isEven = (x: Int) => (x % 2) == 0
    new TestSets {
      assert(!contains(filter(s1, isEven), 1), "Filtering even numbers on {1} should return a set which does NOT contain 1")
      assert(contains(filter(s2, isEven), 2), "Filtering even numbers on {2} should return a set which contains 2")
      assert(!contains(filter(s3, isEven), 3), "Filtering even numbers on {3} should return a set which does NOT contain 3")
    }
  }

  test("forall tests condition on all elements in provided set") {
    val isOdd = (x: Int) => (x % 2) != 0
    val isOneOrTwoOrThree = (x: Int) => (x == 1) || (x == 2) || (x == 3)

    new TestSets {
      val odd = union(s1, s3)
      assert(forall(odd, isOdd), "{1,3} should only contain odd numbers.")
      assert(!forall(s2, isOdd), "{2} should NOT contain only odd numbers.")

      val all = union(union(s1, s2), s3)
      assert(forall(s1, isOneOrTwoOrThree))
      assert(forall(s2, isOneOrTwoOrThree))
      assert(forall(s3, isOneOrTwoOrThree))
      assert(forall(all, isOneOrTwoOrThree))
    }
  }

  test("exists tests whether a set contains at least one element for which the given predicate is true") {
    val isOdd = (x: Int) => (x % 2) != 0

    new TestSets {
      val odd = union(s1, s3)
      val all = union(union(s1, s2), s3)
      val allStartingWith2 = union(union(s2, s1), s3)
      val onlyOneOdd = union(union(s2, s1), singletonSet(4))

      assert(exists(s1, isOdd), "{1} should contain at least one odd numbers.")
      assert(!exists(s2, isOdd), "{2} should NOT contain any odd numbers.")
      assert(exists(s3, isOdd), "{3} should contain at least one odd numbers.")
      assert(exists(odd, isOdd), "{1,3} should contain at least one odd numbers.")
      assert(exists(all, isOdd), "{1,2,3} should contain at least one odd numbers.")
      assert(exists(allStartingWith2, isOdd), "{2,1,3} should contain at least one odd numbers.")
      assert(exists(onlyOneOdd, isOdd), "{2,1,4} should contain at least one odd numbers.")
    }
  }

  test("map transforms a given set into another one by applying to each of its elements the given function.") {
    new TestSets {
      val all = union(union(s1, s2), s3)
      val allTimes2 = map(all, _ * 2)

      assert(!contains(allTimes2, 1), "{2,4,6} should NOT contain 1")
      assert(contains(allTimes2, 2), "{2,4,6} should contain 2")
      assert(!contains(allTimes2, 3), "{2,4,6} should NOT contain 3")
      assert(contains(allTimes2, 4), "{2,4,6} should contain 4")
      assert(!contains(allTimes2, 5), "{2,4,6} should NOT contain 5")
      assert(contains(allTimes2, 6), "{2,4,6} should contain 6")
    }
  }
}
