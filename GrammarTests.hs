module GrammarTests where

import GrammarDef
import Parser
import GrammarParser
import LeftRecA


-- a few test cases

test0 = G "lambda term no direct left recursion" [term, term']
  where term = P "TERM" [[T "var", NT "TERM'"],[T "Lambda", T "var", T "->", NT "TERM", NT "TERM'"]]
        term' = P "TERM'" [[NT "TERM", NT "TERM'"],[Epsilon]]

test1 = G "Lambda" [P "TERM" [[T "var"],[NT "TERM", NT "TERM"],[T "Lambda", T "var", T "->", NT "TERM"]]]

test2 = G "A" [a,b]
  where a = P "A" [[NT "A", NT "A", NT "A"],[NT "B", T "foo"], [T "boo"]]
        b = P "B" [[NT "A"], [T "goo", NT "A"]]

test3 = G "S_indirect" [s, a]
  where s = P "S" [[NT "A", T "a"], [T "b"]]
        a = P "A" [[NT "A", T "c"], [NT "S", T "d"], [T "e"]]

gtest1 = "Sindirect {S -> A a | b, A -> A c | S d | e}"