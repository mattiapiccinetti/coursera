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
      assert(weight(t2) === 9)
    }
  }

  test("frequency of each character in the text") {
    new TestTrees {
      assert(times("".toList) === List())
      assert(times("x".toList) === List(('x', 1)))
      assert(times("xx".toList) === List(('x', 2)))
      assert(times("foo".toList) === List(('f', 1), ('o', 2)))
      assert(times("bar".toList) === List(('b', 1), ('a', 1), ('r', 1)))
      assert(times("bob".toList) === List(('b', 2), ('o', 1)))
      assert(times("puzzle bubble".toList) === List(('p', 1), ('u', 2), ('z', 2), ('l', 2), ('e', 2), (' ', 1), ('b', 3)))
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("check whether the list `trees` contains only one single code tree") {
    assert(!singleton(List()))
    assert(singleton(List(Leaf('x', 42))))
    assert(!singleton(List(Leaf('x', 42), Leaf('y', 1))))
  }

  test("combine of a leaf list with size less than 2") {
    assert(combine(List()) === List())
    assert(combine(List(Leaf('e', 1))) === List(Leaf('e', 1)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("until") {
    val actual = until(singleton, combine)(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4)))
    val expected = List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7))

    assert(actual === expected)
  }

  test("create a simple code tree from a list of char") {
    assert(createCodeTree("foo".toList) === Fork(Leaf('f', 1), Leaf('o', 2), List('f', 'o'), 3))
  }

  test("decode the bit sequence into a list of chars") {
    val tree = Fork(Leaf('f', 1), Leaf('o', 2), List('f', 'o'), 3)
    val bits = List(0, 1, 1)

    assert(decode(tree, bits).mkString === "foo")
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode the text into a list of bits") {
    assert(encode(Fork(Leaf('f', 1), Leaf('o', 2), List('f', 'o'), 3))("foo".toList) === List(0, 1, 1))
  }

  test("codeBits returns the bit sequence that represents the character in the code table") {
    val table = ('x', List(1, 0, 0)) :: ('y', List(1, 1, 0)) :: ('z', List(1, 1, 1)) :: Nil

    assert(codeBits(table)('x') === List(1, 0, 0))
    assert(codeBits(table)('y') === List(1, 1, 0))
    assert(codeBits(table)('z') === List(1, 1, 1))
  }

  test("convert a given tree into a code table") {
    val tree = Fork(Leaf('f', 1), Leaf('o', 2), List('f', 'o'), 3)

    assert(convert(tree) === ('f', List(0)) :: ('o', List(1)) :: Nil)
  }

  test("quickEncode should encode a given text into a list of bits") {
    val tree = Fork(Leaf('f', 1), Leaf('o', 2), List('f', 'o'), 3)
    val text = "foo".toList
    
    assert(quickEncode(tree)(text) === List(0, 1, 1))
  }
}
