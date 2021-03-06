package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: x") {
    assert(wordOccurrences("x") === List(('x', 1)))
  }

  test("wordOccurrences: ffffoo") {
    assert(wordOccurrences("ffffoo") === List(('f', 4), ('o', 2)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("wordOccurrences: PiCcIo") {
    assert(wordOccurrences("PiCcIo") === List(('c', 2), ('i', 2), ('o', 1), ('p', 1)))
  }

  test("wordOccurrences: empty word") {
    assert(wordOccurrences("") === List())
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: foo bar") {
    assert(sentenceOccurrences(List("foo", "bar")) === List(('a', 1), ('b', 1), ('f', 1), ('o', 2), ('r', 1)))
  }

  test("sentenceOccurrences: empty sentence") {
    assert(sentenceOccurrences(List()) === List())
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: ab") {
    val abba = List(('a', 1), ('b', 1))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('b', 1)),
      List(('a', 1), ('b', 1))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: foo") {
    val abba = List(('f', 1), ('o', 2))
    val abbacomb = List(
      List(),
      List(('f', 1)),
      List(('o', 1)),
      List(('o', 2)),
      List(('f', 1), ('o', 1)),
      List(('f', 1), ('o', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }


  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: lard - ar") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val ar = List(('a', 1), ('r', 1))
    val dl = List(('d', 1), ('l', 1))
    assert(subtract(lard, ar) === dl)
  }

  test("subtract: foo - boo == bf") {
    val foo = List(('f', 1), ('o', 2))
    val boo = List(('b', 1), ('o', 2))
    val bf = List(('b', 1), ('f', 1))

    assert(subtract(foo, boo) === bf)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }
}
