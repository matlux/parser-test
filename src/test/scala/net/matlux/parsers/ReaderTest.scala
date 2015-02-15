package net.matlux.parsers

import java.util.UUID
import clojure.lang.Keyword
import org.scalatest.FunSuite
import clojure.lang.Symbol

class ReaderTest extends FunSuite {


  test("list") {
    expectResult(List()) { Reader.readAll("()") }
    expectResult(List(1)) { Reader.readAll("(1)") }
    expectResult(List(1,1)) { Reader.readAll("(1 1)") }
    expectResult(List(List())) { Reader.readAll("(())") }
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



}
