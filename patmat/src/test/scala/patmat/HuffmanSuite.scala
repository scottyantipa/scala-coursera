package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
    val t0 = Leaf('a', 1)
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)


    // From the assignment
    val t3 = Fork(
                Leaf('a', 8),
                Fork(
                  Fork(
                    Leaf('b', 3),
                    Fork(
                      Leaf('c', 1),
                      Leaf('d', 1),
                      "cd".toList,
                      2
                    ),
                    "bcd".toList,
                    5
                  ),
                  Fork(
                    Fork(
                      Leaf('e', 1),
                      Leaf('f', 1),
                      "ef".toList,
                      2
                    ),
                    Fork(
                      Leaf('g', 1),
                      Leaf('h', 1),
                      "gh".toList,
                      2
                    ),
                    "efgh".toList,
                    4
                  ),
                  "bcdefgh".toList,
                  9
                ),
                "abcdefgh".toList,
                17
    )
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

  test("can calc simple occurrence frequency") {
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

 test("can calc more complex occurrence freqency") {
   val timesOfAABAA = times(List('a', 'a', 'b', 'a', 'a'))
   println("timesOfAABAA: ")
   println(timesOfAABAA)
   assert( timesOfAABAA.size == 2 )
   assert( timesOfAABAA.head._1 === 'a')
   assert( timesOfAABAA.head._2 === 4)

   val timesOfAABAACA = times(List('a', 'a', 'b', 'a', 'a', 'c', 'a'))
   println("timesOfAABAACA: ")
   println(timesOfAABAACA)
   assert( timesOfAABAACA.size === 3)
   assert( timesOfAABAACA.head._1 === 'a')
   assert( timesOfAABAACA.head._2 === 5)
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
      assert( decode(t1, List()) == List())
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
      assert( encode(t1)(List()) == List())
      assert( encode(t1)(List('a')) == List(0))
      assert( encode(t1)(List('a', 'a')) == List(0, 0))
      assert( encode(t1)(List('a', 'b')) == List(0, 1))
      assert( encode(t1)(List('a', 'b', 'a', 'b')) == List(0, 1, 0, 1))

      assert( encode(t2)(List('b')) == List(0, 1))

      assert( encode(t3)(List('a')) == List(0))
      assert( encode(t3)(List('a', 'a')) == List(0, 0))
    }
  }

  test("encode long homogenous list") {
    new TestTrees {
      val charList = 0.to(1000000).toList.map(i => 'a')
      val bitList = charList.map(c => 0)
      assert( encode(t3)(charList) === bitList)
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode more complex text") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ababababa".toList)) === "ababababa".toList)
      assert(decode(t1, encode(t1)("aaabbbaaa".toList)) === "aaabbbaaa".toList)
      assert(decode(t2, encode(t2)("aabbddaa".toList)) === "aabbddaa".toList)
      assert(decode(t3, encode(t3)("aabbddaa".toList)) === "aabbddaa".toList)
    }
  }

  test("quickEncode simple") {
    new TestTrees {
      assert( quickEncode(t1)(List('a')) == List(0) )
    }
  }

  test("quickEncode assignment tree") {
    new TestTrees {
      assert( quickEncode(t3)(List('a', 'a')) === List(0, 0))
      assert( quickEncode(t3)(List('h')) === List(1, 1, 1, 1))
      assert( quickEncode(t3)(List('h', 'a')) === List(1, 1, 1, 1, 0))
    }
  }

  test("quickEncode empty list") {
    new TestTrees {
      assert( quickEncode(t3)(List()) === List())
    }
  }

  test("quickEncode super long homogenous list") {
    new TestTrees {
      val charList = 0.to(1000000).toList.map(i => 'a')
      val bitList = charList.map(c => 0)
      assert( quickEncode(t3)(charList) === bitList)
    }
  }

}
