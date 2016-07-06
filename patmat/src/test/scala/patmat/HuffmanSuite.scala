package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }
  //
  //
  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("can calc occurrence frequency") {
    val timesOfEmpty = times(List())
    assert( timesOfEmpty.size === 0 )

    val timesOfA = times(List('a'))
    assert( timesOfA.size === 1 )
    assert( timesOfA.head._1 === 'a' )
    assert( timesOfA.head._2 === 1 )

    val timesOfAAB = times(List('a', 'a', 'b'))
    assert( timesOfAAB.size === 2 )
    assert( timesOfAAB.head._1 === 'a' )
    assert( timesOfAAB.head._2 == 2 )
    assert( timesOfAAB.tail.head._1 == 'b' )
    assert( timesOfAAB.tail.head._2 == 1 )
  }



  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("make code tree") {
    assert( createCodeTree(List('a')) == Leaf('a', 1))
    assert( createCodeTree(List('a', 'a', 'b')) == Fork(Leaf('b',1),Leaf('a',2),List('b', 'a'),3) )
  }

  test("decode") {
    new TestTrees {
      assert( decode(t1, List(0)) == List('a'))
      assert( decode(t1, List(1)) == List('b'))
      assert( decode(t2, List(1)) == List('d'))
      assert( decode(t2, List(1)) == List('d'))
      assert( decode(t2, List(0, 1)) == List('b'))

      println("decoded secret: ", decodedSecret.toString)
    }
  }

  test("encode") {
    new TestTrees {
      assert( encode(t1)(List('a')) == List(0))
      assert( encode(t1)(List('a', 'a')) == List(0, 0))
      assert( encode(t1)(List('a', 'b')) == List(0, 1))
      assert( encode(t1)(List('a', 'b', 'a', 'b')) == List(0, 1, 0, 1))

      assert( encode(t2)(List('b')) == List(0, 1))
    }
  }



  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quickEncode") {
    new TestTrees {
      assert( quickEncode(t1)(List('a')) == List(0) )
    }
  }

}
