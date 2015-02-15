package net.matlux.parsers

import java.util.UUID
import clojure.lang.Keyword
import org.scalatest.FunSuite
import clojure.lang.Symbol

class ReaderTest extends FunSuite {

  test("set") {
    expectResult(Set()) { Reader.readAll("#{}") }
    expectResult(Set(1)) { Reader.readAll("#{1}") }
    expectResult(Set(1)) { Reader.readAll("#{1 1}") }
    expectResult(Set(Set())) { Reader.readAll("#{#{}}") }
  }

  test("vector") {
    expectResult(Vector()) { Reader.readAll("[]") }
    expectResult(Vector(1)) { Reader.readAll("[1]") }
    expectResult(Vector(1,1)) { Reader.readAll("[1 1]") }
    expectResult(Vector(Vector())) { Reader.readAll("[[]]") }
    expectResult(Vector(Vector(), Set())) { Reader.readAll("[[] #{}]") }
  }

  test("list") {
    expectResult(List()) { Reader.readAll("()") }
    expectResult(List(1)) { Reader.readAll("(1)") }
    expectResult(List(1,1)) { Reader.readAll("(1 1)") }
    expectResult(List(List())) { Reader.readAll("(())") }
    expectResult(List(List(), List(), Set())) { Reader.readAll("(() [] #{})") }
  }

  test("keyword") {
    expectResult(Keyword.intern("a")) { Reader.readAll(":a") }
    expectResult(Keyword.intern(":a")) { Reader.readAll("::a") }
    expectResult(Keyword.intern("foo/bar")) { Reader.readAll(":foo/bar") }
  }

  test("symbols") {
    expectResult(clojure.lang.Symbol.intern("a")) { Reader.readAll("a") }
    expectResult(clojure.lang.Symbol.intern("f")) { Reader.readAll("f") }
    expectResult(clojure.lang.Symbol.intern("foo/bar")) { Reader.readAll("foo/bar") }

    expectResult(List(Keyword.intern("a"),Keyword.intern("b"),Keyword.intern("c"))) { Reader.readAll("(:a :b :c)") }
    expectResult(List(Symbol.create("a"),Symbol.create("b"),Symbol.create("c"))) { Reader.readAll("(a b c)") }
  }


  test("map") {
    expectResult(Map()) { Reader.readAll("{}") }
    expectResult(Map(Keyword.intern("a") -> 1)) { Reader.readAll("{:a 1}") }
    expectResult(Map(Keyword.intern("a") -> 1, Keyword.intern("b") -> 2)) { Reader.readAll("{:a 1 :b 2}") }
    expectResult(Map(Map() -> Map())) { Reader.readAll("{{} {}}") }
    expectResult(Map(Map() -> Map(), Set() -> Vector(), List() -> Keyword.intern("a")))
    { Reader.readAll("{{} {} #{} [] () :a}") }
  }

  test("true / false / nil ") {
    expectResult(true) { Reader.readAll("true") }
    expectResult(false) { Reader.readAll("false") }
    expectResult(null) { Reader.readAll("nil") }
    expectResult(List(true, false, List(null))) { Reader.readAll("(true false (nil))") }
  }

  test("#uuid") {
    val uuidStr = "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"
    expectResult(UUID.fromString(uuidStr)) { Reader.readAll("#uuid\"" + uuidStr + "\"") }
    expectResult(List(UUID.fromString(uuidStr))) { Reader.readAll("(#uuid\"" + uuidStr + "\")") }
  }

  test("#inst") {
    val dateStr = "2012-01-01T01:23:45.000-00:00"
    expectResult(Instant.read(dateStr)) { Reader.readAll("#inst \"" + dateStr + "\"") }
    expectResult(List(Instant.read(dateStr))) { Reader.readAll("(#inst \"" + dateStr + "\")") }
  }

  test("commas") {
    expectResult(1) { Reader.readAll(",,,1") }
    // expectResult(1) { Reader.readAll("1,,,") }   // TODO FIX
    expectResult(Vector(1,2,3)) { Reader.readAll("[1,2,3]") }
    expectResult(Map(Keyword.intern("a") -> 1, Keyword.intern("b") -> 2)) { Reader.readAll("{:a 1, :b 2}") }
  }

  test("strings") {
    intercept[RuntimeException] { Reader.readAll("") }
    expectResult("foo") { Reader.readAll("\"foo\"") }
    expectResult(Map("foo" -> Vector("bar"))) { Reader.readAll{"{\"foo\" [\"bar\"]}"} }
  }

  // non edn spec stuff (still accepted by 'clojure.edn)

  test("ratio") {
    expectResult(0.5) { Reader.readAll("1/2") }
    expectResult(Set(0.5, 1)) { Reader.readAll("#{1/2 42/42}") }
  }

  test("numbers with N") {
    expectResult(List(42, 1)) { Reader.readAll("(42 1)") }
    expectResult(List(42.5, 1)) { Reader.readAll("(42.5 1)") }
  }
}
